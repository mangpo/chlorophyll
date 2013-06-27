#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-desugar.rkt")

(provide (all-defined-out))

;; 1) Attach data type to every variable, and function signature AST to function call.
;; 2) Bind expect and expand.
;; 3) Interpret known/unknown type.
;; 4) Mark array is cluster or not cluster.
;; 5) Expand place. int::2@?? -> int::2@(??,??)
(define linker%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] 
                [array-map (make-hash)] 
                [non-native #f] 
                [entry #f]
                [stmt-level #f])
    ;; env maps
    ;; 1) var-name  -> (cons type known)
    ;; 2) func-name -> func-ast

    ;; type
    ;; 1) data-type
    ;; 2) (cons data-type entry)

    ;; Declare IO function: in(), out(data)
    (declare env "in" (get-stdin))
    (declare env "out" (get-stdout))

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
	    (cons place (for/list ([i (in-range (sub1 n))]) (get-sym)))]
	   
	   [(is-a? place Place%)
	    (let* ([at (get-field at place)]
		   [at-list
		    (if (is-a? at Exp%)
			(send at accept desugarer)
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
               (set! non-native #t)
               (set-field! type ast (car type))
               (set-field! expect ast (cdr type)))
             (set-field! expect ast 1))
         
         (define known (get-field known ast))  
         (for ([name (get-field var-list ast)])
           ;; declare type
           (declare env name 
		    (if (string? type)
			(val type 1 known)
			(val (car type) (cdr type) known))))

	 (define expect (get-field expect ast))
	 (define place (get-field place ast))
	 (when (and (> expect 1) (not (is-a? place TypeExpansion%)))
	       (set-field! place ast (new TypeExpansion% [place-list (expand-place place expect)])))
         ]
        
        [(is-a? ast ArrayDecl%)
         ;(pretty-display (format "LINKER: VarDecl ~a" (get-field var ast)))
         (define type (get-field type ast)) 
	 (if (pair? type)
             (begin
               (visit-place (get-field place-list ast) (cdr type))
               (set! non-native #t)
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
	 (define place (get-field place-list ast))
	 (when (and (> expect 1) (not (is-a? place TypeExpansion%)))
	       (set-field! place-list ast (new TypeExpansion% [place-list (expand-place place expect)])))
	 ]
        
        [(is-a? ast Num%)
         ;(pretty-display (format "LINKER: Num ~a" (send ast to-string)))
         (set-field! expect ast entry)
         #t]
        
        [(is-a? ast Array%)
         ;(pretty-display (format "LINKER: Array ~a" (send ast to-string)))
	 (set-field! expect ast entry)

	 (set! entry 1)
	 (define index (get-field index ast))
         (define index-known (send index accept this))
	 (set! entry (get-field expect ast))

         (unless index-known
           (set-field! cluster (lookup array-map ast) #t))
         
         (define pack (lookup env ast))
         (define type (val-type pack))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
         (when (number? (get-field sub ast))
               (set! expand 1))
	 (when (> expand entry)
	       (send ast partition-mismatch expand entry))

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
				 (new TypeExpansion% [place-list (expand-place place entry)])))
	       )
	 "int"]
        
        [(is-a? ast UnaExp%)
         ;(pretty-display (format "LINKER: UnaExp ~a" (send ast to-string)))
	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
         (define e1-known (send e1 accept this))
	 
	 (define op (get-field op ast))
	 (set-field! place-type ast (get-field place op))

         (define op-type (send op accept this))
	 (set-field! type ast op-type)
	 e1-known]
        
        [(is-a? ast BinExp%)
         ;(pretty-display (format "LINKER: BinExp ~a" (send ast to-string)))
	 (set-field! expect ast entry)
	 (define e1 (get-field e1 ast))
	 (define e2 (get-field e2 ast))
         (define e1-known (send e1 accept this))
         (define e2-known (send e2 accept this))
	 
	 (define op (get-field op ast))
	 (set-field! place-type ast (get-field place op))

         (define op-type (send op accept this))
	 (set-field! type ast op-type)
	 (and e1-known e2-known)]
        
        [(is-a? ast FuncCall%)
         ;(pretty-display (format "LINKER: FuncCall ~a" (send ast to-string)))
         (define func-ast (lookup env ast))
         (define type (get-field type (get-field return func-ast)))

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
         (for ([arg args]
	       [param params])
	      (let ([param-type (get-field type param)])
		(if (string? param-type)
		    (set! entry 1)
		    (set! entry (cdr param-type)))
		(let ([arg-known (send arg accept this)])
		  (set-field! known-type param (and (get-field known-type param) arg-known)))))
	        
	 (set! entry old-entry)
	 #f]

	[(is-a? ast Send%)
	 (set! entry 1)
	 (send (get-field data ast) accept this)]

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

	 (when (and lhs-known (not rhs-known))
               (set-val-known! (lookup env lhs) #f))]

	[(is-a? ast Return%)
	 (define pack (lookup-name env "#return"))
	 (define expand (val-expand pack))
         (define known-type (val-known pack))
	 
	 (set! entry expand)
	 (set-field! expect ast entry)

         (set! stmt-level #f)
	 (send (get-field val ast) accept this)
	 ]
        
        [(is-a? ast If%)
         ;(pretty-display "LINKER: If")
         (set! entry 1)
         (set! stmt-level #f)
         (send (get-field condition ast) accept this)

	 (push-scope)
	 (send (get-field true-block ast) accept this)
	 (pop-scope)

	 (when (get-field false-block ast)
	       (push-scope)
	       (send (get-field false-block ast) accept this)
	       (pop-scope))
         ]
        
        [(is-a? ast While%)
         ;(pretty-display "LINKER: While")
         (set! entry 1)
         (set! stmt-level #f)
         (send (get-field condition ast) accept this)
	 (push-scope)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [(is-a? ast For%)
         ;(pretty-display "LINKER: For")
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

         ;; return non-native flag
         non-native
         ]
        
        [(is-a? ast Block%)
         ;(pretty-display "LINKER: Block")
         (for ([stmt (get-field stmts ast)])
	      (set! entry #f)
              (set! stmt-level #t)
	      (send stmt accept this))
         ]
        
        
        [(is-a? ast FuncDecl%)
	 (push-scope)
         (define return (get-field return ast))
	 (send return accept this)

         ;; reserve expanded type for return value
         (define entry (get-field expect return))
         (when (> entry 1)
             (set-field! type return (cons (get-field type return) entry)))

	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [else
         (raise (format "visitor-linker: unimplemented for ~a" ast))]
        
        ))))
  

