#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" 
         "visitor-interface.rkt" "visitor-desugar.rkt" "visitor-lowerbound.rkt")

(provide (all-defined-out))

;; 1) Attach data type to every variable, and function signature AST to function call.
;; 2) Bind expect and expand.
;; 3) Interpret known/unknown type.
;; 4) Mark array is cluster or not cluster.
;;    Use known-type to determine if array should be clustered or not.
;; 5) Expand place. int::2@?? -> int::2@(??,??)
(define linker%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] 
                [array-map (make-hash)] 
                [entry #f]
                [stmt-level #f]
                [lowerbound (new lowerbound%)])
    ;; env maps
    ;; 1) var-name  -> (cons type known)
    ;; 2) func-name -> func-ast

    ;; type
    ;; 1) data-type
    ;; 2) (cons data-type entry) for int::2

    ;; Declare IO function: in(), out(data)
    (declare env "in" (get-stdin))
    (declare env "out" (get-stdout))
    (for ([node digital-nodes])
      (declare env (format "digital_read~a" node) (get-digital-read node)))
    (for ([node (append digital-nodes analog-nodes)])
      (declare env (format "set_io~a" node) (get-set-io node))
      (declare env (format "digital_wakeup~a" node) (get-digital-wakeup node))
      (declare env (format "delay_ns~a" node) (get-delay-ns node))
      (declare env (format "delay_unext~a" node) (get-delay-unext node)))

    (struct val (type expand known) #:mutable)

    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" array-map)
        (set! array-map new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__"))
      (set! array-map (dict-ref array-map "__up__")))

    (define (visit-place place n)
      (when (and (is-a? place Place%) (is-a? (get-field at place) Exp%))
	      (send (get-field at place) accept this)))

    ;; Return list of n places
    (define (expand-place place n)
      (define (different place)
	(map (lambda (x) (new RangePlace% 
			      [place (get-sym)] 
			      [from (get-field from x)]
			      [to (get-field to x)]))
	     place))
      
      (if (= n 1)
	  (when (and (is-a? place Place%) (is-a? (get-field at place) Exp%))
		(send (get-field at place) accept desugarer))
	  (cond
	   [(is-a? place TypeExpansion%)
	    (get-field place-list place)]
	   
	   [(symbolic? place)
            ;; if symbolic, create n different symbolic variables.
	    (cons place (for/list ([i (in-range (sub1 n))]) (get-sym)))]
	   
	   [(is-a? place Place%)
	    (let* ([at (get-field at place)]
		   [at-list
		    (if (is-a? at Exp%)
                        ;; desugarer will expand according to 'expect' field
			(send at accept desugarer)
                        ;; if not, expand munually
			(for/list ([i (in-range n)]) at))])
	      (map (lambda (x) (new Place% [at x])) at-list))]
	   
	   [(place-type-dist? place)
	    (cons place 
		  (for/list ([i (in-range (sub1 n))]) 
			    (cons (different (car place)) (cdr place))))]
	   
	   [(list? place)
	    (cons place 
		  (for/list ([i (in-range (sub1 n))]) 
			    (different place)))]
	   
	   [else
	    (raise (format "visitor-linker: expand-place: unimplemented for ~a" place))])))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         ;(pretty-display (format "LINKER: VarDecl ~a" (get-field var-list ast)))
         (define type (get-field type ast)) 
	 (if (pair? type)
             (begin
               (visit-place (get-field place ast) (cdr type))
               (set-field! type ast (car type))
               (set-field! expect ast (cdr type)))
             (set-field! expect ast 1))
         
         (define known (get-field known ast))  
         (for ([name (get-field var-list ast)])
           ;; declare type
           (declare env name 
		    (if (pair? type)
			(val (car type) (cdr type) known)
			(val type 1 known))))

	 (define expect (get-field expect ast))
	 (define place (get-field place ast))
	 (when (and (> expect 1) (not (is-a? place TypeExpansion%)))
	       (set-field! place ast (new TypeExpansion% [place-list (expand-place place expect)])))
         ast
         ]
        
        [(is-a? ast ArrayDecl%)
         ;(pretty-display (format "LINKER: VarDecl ~a" (get-field var ast)))
         (define type (get-field type ast)) 

         ;; Desugar block-layout
         (define block (get-field place-list ast))
         (when (is-a? block BlockLayout%)
               (define array-bound (get-field bound ast))
               (define block-size (get-field size block))
               (define n (ceiling (/ array-bound block-size)))
               (define last-size (- array-bound (* block-size (sub1 n))))
               (define places (get-field place-list block))
               (define rangeplace-list 
                 (append 
                  (for/list ([i (in-range (sub1 n))])
                            (new RangePlace% 
                                 [from (* i block-size)]
                                 [to (* (add1 i) block-size)]
                                 [place #f]))
                  (list (new RangePlace% 
                             [from (* (sub1 n) block-size)]
                             [to array-bound]
                             [place #f]))))
               (cond
                [(equal? places #f)
                 (for ([rangeplace rangeplace-list])
                      (set-field! place rangeplace (get-sym)))
                 (set-field! place-list ast rangeplace-list)]

                [(list? places)
                 (unless (= (length places) n)
                         (raise (format "Number of places specified in the annotation has to be equal to array-size/block-size. Error at '~a' array declaration at line ~a"
					(get-field var ast) (send ast get-line))))
                 (for ([rangeplace rangeplace-list]
                       [place places])
                      (set-field! place rangeplace place))
                 (set-field! place-list ast rangeplace-list)]
                
                [(is-a? places TypeExpansion%)
                 ;; trun list of numbers into list of RangePlaces
                 (set-field! 
                  place-list places 
                  (for/list ([p-list (get-field place-list places)])
                    (unless (= (length p-list) n)
                            (raise (format "Number of places specified in the annotation has to be equal to array-size/block-size. Error at '~a' array declaration at line ~a"
					   (get-field var ast) (send ast get-line))))
                    (for/list ([rangeplace rangeplace-list]
                               [place p-list])
                              (new RangePlace%
                                   [from (get-field from rangeplace)]
                                   [to (get-field to rangeplace)]
                                   [place place]))))
                 ;; set block's TypeExpansion to ast's TypeExpansion
                 (set-field! place-list ast places)
                 ]))           

	 (if (pair? type)
             (begin
               (visit-place (get-field place-list ast) (cdr type))
               (set-field! type ast (car type))
               (set-field! expect ast (cdr type)))
             (set-field! expect ast 1))
           
         ;; declare type
         (declare env (get-field var ast) 
		  (if (string? type)
		      (val type 1 #t)
		      (val (car type) (cdr type) #t)))
         (declare array-map (get-field var ast) ast)

	 (define expect (get-field expect ast))
         (when (rosette-number? (get-field place-list ast))
               (set-field! place-list ast 
                           (list (new RangePlace% 
                                      [from 0] [to (get-field bound ast)]
                                      [place (get-field place-list ast)]))))

	 (define place (get-field place-list ast))
	 (when (and (> expect 1) (not (is-a? place TypeExpansion%)))
	       (set-field! place-list ast (new TypeExpansion% [place-list (expand-place place expect)])))
         ast
	 ]
        
        [(is-a? ast Num%)
         ;(pretty-display (format "LINKER: Num ~a" (send ast to-string)))
         (set-field! expect ast entry)
         #t]
        
        [(is-a? ast Array%)
         ;(pretty-display (format "LINKER: Array ~a" (send ast to-string)))
	 (set-field! expect ast entry)

	 ;(set! entry 1)
	 (define index (get-field index ast))
         (define index-known (send index accept this))
	 ;(set! entry (get-field expect ast))

         (define array-decl (lookup array-map ast))
         (unless index-known
           (set-field! cluster array-decl #t))
         (send array-decl update-compress (send index accept lowerbound))
         
         (define pack (lookup env ast))
         (define type (val-type pack))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
         (define place-type (get-field place-type ast))
         (when (number? (get-field sub ast))
               (set! expand 1))
	 (when (> expand entry)
	       (send ast partition-mismatch expand entry))
         (when (and (> entry 1) place-type)
               ;; expand for ghost region tuple type
               (visit-place place-type entry)
	       (when (not (is-a? place-type TypeExpansion%))
                     (set-field! place-type ast 
				 (new TypeExpansion% 
                                      [place-list (expand-place place-type entry)]))))

	 (set-field! type ast type)
	 (set-field! expand ast expand)
	 (and known-type index-known)]
        
        [(is-a? ast Var%)
         ;(pretty-display (format "LINKER: Var ~a" (send ast to-string)))
	 (set-field! expect ast entry)

         (define pack (lookup env ast))
         (define type (val-type pack))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))

         (when (number? (get-field sub ast))
               (set! expand 1))

         ;(pretty-print `(expand ,expand entry ,entry))
	 (when (> expand entry)
	       (send ast partition-mismatch expand entry))

	 (set-field! type ast type)
	 (set-field! expand ast expand)
	 known-type]

	[(is-a? ast Op%)
         (set-field! expect ast entry)
         (when (> entry 1)
	       (visit-place (get-field place ast) entry)
	       (define place (get-field place ast))
	       (when (not (is-a? place TypeExpansion%))
		     (set-field! place ast 
				 (new TypeExpansion% [place-list (expand-place place entry)]))))]
        
        [(is-a? ast UnaExp%)
         ;(pretty-display (format "LINKER: UnaExp ~a" (send ast to-string)))
	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
         (define e1-known (send e1 accept this))
	 
	 (define op (get-field op ast))
	 (set-field! place-type ast (get-field place op))

         (send op accept this)
	 (set-field! type ast (get-field type e1))
	 e1-known]
        
        [(is-a? ast BinExp%)
         ;(pretty-display (format "LINKER: BinExp ~a" (send ast to-string)))
	 (when (member (get-field op (get-field op ast)) (list "/%" "*:2" ">>:2" ">>>"))
	       (if (= entry 2)
		   (set! entry 1)
		   (raise (format "visitor-linker: return value entries mismatched at ~a l:~a"
				  (send ast to-string) (send ast get-line)))))

	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
	 (define e2 (get-field e2 ast))
         (define e1-known (send e1 accept this))
         (define e2-known (send e2 accept this))
         (define e1-type (get-field type e1))
         (define e2-type (get-field type e2))
	 
	 (define op (get-field op ast))
         (define op-str (get-field op op))
	 (set-field! place-type ast (get-field place op))

         (send op accept this)

         (cond
          [(and (is-a? e1 Num%) (is-a? e2 Num%)) (set-field! type ast "int")]

          [(member op-str (list "+" "-" "*" "!=" "==" "<" ">" ">=" "<="))
           (cond
            [(is-a? e1 Num%)
             (when (fix_t? e2-type)
                   (send e1 set-value (d2fp (send e1 get-value) (fix_t-int e2-type)))
                   (set-field! type e1 e2-type)
                   )
             (set-field! type ast e2-type)]

            [(is-a? e2 Num%)
             (when (fix_t? e1-type)
                   (send e2 set-value (d2fp (send e2 get-value) (fix_t-int e1-type)))
                   (set-field! type e2 e1-type)
                   )
             (set-field! type ast e1-type)]

            [else
             ;; TODO: add conversion
             (unless (same-type? e1-type e2-type)
                     (raise (format "visitor-linker: operands of ~a have different types." op-str)))
             (set-field! type ast e1-type)
             ])
           ]

          [(member op-str (list "<<" ">>"))
           (unless (equal? e2-type "int")
                   (raise (format "visitor-linker: 2nd operand of ~a should be 'int' but ~a is given." op-str e2-type)))
           (set-field! type ast e1-type)]

          [else
           (unless (and (equal? e1-type "int") (equal? e2-type "int"))
                   (raise (format "visitor-linker: ~a only accepts operands of type 'int'." op-str)))
           (set-field! type ast "int")])

	 (and e1-known e2-known)]
        
        [(is-a? ast FuncCall%)
         ;;(pretty-display (format "LINKER: FuncCall ~a" (send ast to-string)))
	 (when (io-func? (get-field name ast))
           (let ([node (send (car (get-field args ast)) get-value)])
             (set-field! name ast (format "~a~a" (get-field name ast) node))
             (set-field! args ast (cdr (get-field args ast)))))

         (define func-ast (lookup env ast))
         (define type (if (get-field return func-ast)
                          (get-field type (get-field return func-ast))
                          "void"))

         (when (and entry (equal? type "void"))
               (send ast type-error))

         ;; set signature
         (set-field! signature ast func-ast)
         (set-field! is-stmt ast stmt-level)

	 ;; set type and expand
	 (if (string? type)
	     (begin
	       (set-field! type ast type)
	       (set-field! expand ast 1))
	     (begin
	       (set-field! type ast (car type))
	       (set-field! expand ast (cdr type))))

	 ;; set expect
	 (if entry
	     (set-field! expect ast entry)
	     (set-field! expect ast (get-field expand ast)))

	 ;; check expand against expect
	 (when (> (get-field expand ast) (get-field expect ast))
	       (send ast partition-mismatch (get-field expand ast) (get-field expect ast)))
         
         (define args (get-field args ast))
         (define params (get-field stmts (get-field args func-ast)))

         (unless (= (length args) (length params))
                 (send ast args-mismatch (length params)))
         
         (define old-entry entry)
         (set! stmt-level #f)
         (for ([arg args]
	       [param params])
	      (let ([param-type (get-field type param)])
		(if (string? param-type)
		    (set! entry 1)
		    (set! entry (cdr param-type)))
		(let ([arg-known (send arg accept this)])
		  (set-field! known-type param (and (get-field known-type param) arg-known)))))
	        
	 (set! entry old-entry)
         (if (get-field is-stmt ast)
             ast
             #f)]

	[(is-a? ast Send%)
	 (set! entry 1)
	 (send (get-field data ast) accept this)
         ast]

	[(is-a? ast Recv%)
	 #f]
        
        [(is-a? ast Assign%)
         ;(pretty-display (format "LINKER: Assign ~a ~a" lhs rhs))
         (define lhs (get-field lhs ast))
         (define rhs (get-field rhs ast))
         (define lhs-pack (lookup env lhs))
         (define lhs-expand (val-expand lhs-pack))
         ;(pretty-display `(lhs-expand ,lhs-expand))

         (if (number? (get-field sub lhs))
             (set! entry 1)
             (set! entry lhs-expand))
         
         (set! stmt-level #f)
	 (define lhs-known (send lhs accept this))
	 (define rhs-known (send rhs accept this))
	 (define lhs-type (get-field type lhs))
	 (define rhs-type (get-field type rhs))

         (when (not (same-type? lhs-type rhs-type))
               (if (and (fix_t? lhs-type) (is-a? rhs Num%))
                   (begin
                     (send rhs set-value (d2fp (send rhs get-value) (fix_t-int lhs-type)))
                     (set-field! type rhs lhs-type))
                   (raise (format "visitor-linker: cannot convert ~a to ~a at Assigned%" rhs-type lhs-type))))

	 (when (and lhs-known (not rhs-known))
               (set-val-known! (lookup env lhs) #f))
         ast]

	[(is-a? ast Return%)
	 (define pack (lookup-name env "#return"))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
	 
	 (set! entry expand)
	 (set-field! expect ast entry)

         (set! stmt-level #f)
	 (send (get-field val ast) accept this)
         ast
	 ]
        
        [(is-a? ast If%)
         ;(pretty-display "LINKER: If")
	 (define exp (get-field condition ast))
	 (define t (get-field true-block ast))
	 (define f (get-field false-block ast))
	 (define place (and (is-a? exp BinExp%) (get-field place (get-field op exp))))

         ;; modify structure of if
         (define new-if
	   (cond
	    [(binop-equal? exp "!=")
	     (new If!=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp "==")
	     (new If!=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [(binop-equal? exp "<")
	     (new If<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp ">")
	     (new If<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp ">=")
	     (new If<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [(binop-equal? exp "<=")
	     (new If<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [else
	     ast]))

         (set! entry 1)
         (set! stmt-level #f)
         (send (get-field condition new-if) accept this)

	 (push-scope)
	 (send t accept this)
	 (pop-scope)

	 (when (get-field false-block ast)
	       (push-scope)
	       (send f accept this)
	       (pop-scope))

         new-if
         ]
        
        [(is-a? ast While%)
         ;(pretty-display "LINKER: While")
         (set! entry 1)
	 (define exp (get-field condition ast))
         (send exp accept this)
	 (define t (get-field body ast))
	 (define place (and (is-a? exp BinExp%) (get-field place (get-field op exp))))

         (define new-while
	   (cond
	    [(binop-equal? exp "!=")
	     (new While!=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "==")
	     (new While==0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "<")
	     (new While<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp ">")
	     (new While<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp ">=")
	     (new While>=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "<=")
	     (new While>=0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [body t])]
	    
	    [else
	     ast]))

         (set! entry 1)
         (set! stmt-level #f)
         (send (get-field condition new-while) accept this)
	 (push-scope)
         (send t accept this)
	 (pop-scope)

         new-while
         ]
        
        [(is-a? ast For%)
         ;(pretty-display "LINKER: For")
         (when (<= (get-field to ast) (get-field from ast))
               (raise "useless For% at line ~a" (send ast get-line)))

	 (push-scope)
	 (declare env (get-field name (get-field iter ast)) (val "int" 1 #t))
         (send (get-field body ast) accept this)
	 (pop-scope)
         ;; TODO
         ;; int::2[] a[10];
         ;; for i {
         ;;   if(i % 2 == 0) {
         ;;     a[i] = ..
         ;;   } 
         ;; }
         ast
         ]
        
        [(is-a? ast Program%)
         ;(pretty-display "LINKER: Program")
         (define stmts (get-field stmts ast))
         
         ;; update env front to back
         (for ([stmt stmts])
	      (if (is-a? stmt FuncDecl%)
		  (declare env (get-field name stmt) stmt)
		  (send stmt accept this)))
         
         ;; desugar back to front
         (for ([stmt (reverse stmts)])
           (when (is-a? stmt FuncDecl%)
		 (send stmt accept this)))
         ]
        
        [(is-a? ast Block%)
         ;(pretty-display "LINKER: Block")
         (set-field! stmts ast
                     (map (lambda (stmt)
                            (set! entry #f)
                            (set! stmt-level #t)
                            (send stmt accept this))
                          (get-field stmts ast)))
         ast
         ]
        
        [(is-a? ast FuncDecl%)
	 (push-scope)
         (define return (get-field return ast))
	 (when return 
               (send return accept this)

               ;; reserve expanded type for return value
               (define entry (get-field expect return))
               (when (> entry 1)
                     (set-field! type return (cons (get-field type return) entry))))

	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [else
         (raise (format "visitor-linker: unimplemented for ~a" ast))]
        
        ))))
  

