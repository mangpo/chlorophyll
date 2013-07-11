#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "ast-util.rkt"
         "parser.rkt" 
	 "partition-storage.rkt"
         "space-estimator.rkt"
         "visitor-interface.rkt" 
         "visitor-desugar.rkt")

(provide count-msg-interpreter% (struct-out comminfo))

(struct comminfo (msgs placeset))

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field places
                [env (make-hash)] ;; map varname -> place, arrayname -> (place cluster)
                [has-func-temp #f]
		)

    (define debug #f)
    (define debug-sym #f)
    
    ;; Declare IO function: in(), out(data)
    (declare env "in" (comminfo 0 (set)))
    (declare env "out" (comminfo 0 (set)))
    
    ;; find actual place for @place(exp)
    (define (find-place ast [modify #t])
      ;(pretty-display `(find-place ,ast))
      (when (not (is-a? ast Livable%))
        (raise "find-place: ast is not Livable%"))
      
      (define place-exp (get-field place ast))

      (define (derive-place p)
        ;(pretty-display `(derive-place ,p))
        (cond
         [(or (equal? p "any") (equal? p "io"))
          p]

         [(is-a? p Array%)
          (car (lookup env p))]

         [(is-a? p Var%) 
          (lookup env p)]
         
         [else
          (pretty-display "raise error")
          (raise (format "derive-place: unimplemented for ~a" p))]))

      (cond
        [(at-io? place-exp)
         place-exp]
        [(is-a? place-exp Place%)
         (let ([place (derive-place (get-field at place-exp))])
           (when modify (set-field! place ast place))
           place)]
        [else
         place-exp]))
    
    ;; find index of @place(x[i]) if x is distributed.
    (define (find-index place)
      (if (is-a? place Place%)
          (let* ([at (get-field at place)]
                 [index (if (is-a? at Array%)
			    ;; return i when place = @place(x[i])
			    (get-field index at)
                            ;; return i when place = @place(i)
                            at)])
	    (define info (lookup env at))
	    ;(pretty-display `(find-index at ,(send at to-string) info ,info))
	    (when (and (pair? info) (not (list? info)) (cdr info)) 
		  ;; @place(x[i]) if x is cluster, illegal
		  (send place illegal-place))
            (if (is-a? index Var%)
                  index
                (raise "Place expression cannot be more complicated than @place(x[i])")))         
          ;; return false otherwise
          #f))

    ;; find place-type for @place(x)
    (define (find-place-type ast native)
      (when (not (is-a? native Livable%))
        (raise "find-place-type: native is not Livable%"))
      
      (define place-type (get-field place-type ast))
      (if (or (number? place-type) (pair? place-type))
          ; place-type is infered during abstract interpretation
          ; and the format is right, we can return right away
          place-type
          (begin
            ; get index for @place(i) before place because (find-place) will remove that info
            (define index (find-index (get-field place native)))
            (define place (find-place native))
	    ;(pretty-display `(find-place-type place ,place index ,index))
            (if (or (number? place) (place-type-dist? place))
                place
                (cons place index)))))

    ;; env dictionary map variable x to
    ;; 1) place if x is not array
    ;; 2) (place . cluster) if x is array
    ;;    cluster is either true or false

    ;; place has 2 categories
    ;; 1) single place => integer
    ;; 2) place list   => (list_of_RangePlace%)

    ;; place-type has 2 categories
    ;; 1) single place => integer
    ;; 2) place list & index => (list_of_RangePlace% . index)

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      ;(assert (or (number? place) (list? place)))
      (cond
        [(number? place)
         (cores-inc-space places place add-space)]
        
        [(at-io? place)
         void]
        
        [else
         (let ([place-list 
                (if (place-type-dist? place) (car place) place)])
           (for ([p place-list])
		(cores-inc-space places (get-field place p) add-space)))]))
    
    ;;; Increase the used space of "place" with op.
    (define (inc-space-with-op place op)
      (assert (or (number? place) (list? place)))
      ;TODO: change to (cores-add-op places place op)
      (if (number? place)
          (cores-inc-space places place (est-space op))
          (let ([place-list
                 (if (place-type-dist? place) (car place) place)])
            (for ([p place-list])
                 (cores-inc-space places (get-field place p) (est-space op))))))
    
    ;; Increase the used space of places in the given set by "add-space".
    ;; Used in body inside for and if.
    (define (inc-space-placeset placeset add-space)
      (define (loop placelist)
        (when (not (empty? placelist))
          (let* ([others (cdr placelist)]
                 [me (car placelist)])
            (loop others)
            (when (andmap (lambda (x) (not (equal? x me))) others) 
                  (inc-space me add-space)))))
        
      (loop (set->list placeset)))

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg-place-type x y x-ast [y-ast #f])
      (when debug
      	    (pretty-display `(count-msg-place-type ,x ,y)))
      ;(assert (and (is-a? x-ast Base%) (is-a? y-ast Base%)))

      ;; Return the place that a resides if a only lives in one place. 
      ;; Otherwise, return string representation of a.
      (define (get-str-rep a)
        ;(assert (place-type? a))
	(if (number? a)
	    a
            (let ([place-list (car a)]
                  [index (cdr a)])
              (if (= (length place-list) 1)
                  (get-field place (car place-list))
                  (format "~a . ~a" 
                          (place-list-to-string place-list)
                          (send index to-string))))))

      ;; Add comm space to cores
      (define (add-comm x)
	;(assert (place-type? x))
        (if (number? x)
             (inc-space x est-comm)
             (for ([p (car x)])
                  (inc-space (get-field place p) est-comm))))

      ;; Return 1 if it is in one place or it is a non-cluster array.
      ;; Return number of cores p resides in otherwise.
      (define (count-comm p p-ast)
	;(pretty-display `(count-comm ,p ,(send p-ast to-string)))
	;(assert (place-type? p))
        (cond
          [(or (number? p) (equal? p #f)) 1]
          [(is-a? p Place%)
           (let ([at (get-field at p)])
             (if (or (equal? at "any") (equal? at "io"))
                 1
                 (raise "count-comm doesn't support Place% that is not @any")))]
          [(pair? p)
           (if (get-field cluster p-ast)
	       (length (car p))
	       1)]
          [else (raise (format "visiter-interpretor: count-comm: unimplemented for ~a" p))]))
      
      (define x-comm (count-comm x x-ast))
      
      (define y-comm (count-comm y y-ast))

      (cond 
        ;; if x and y are the same place.
        [(same-place? x y) 0]

	;; if x and y are in a single core.
	[(and (= x-comm 1) (= y-comm 1))
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display "COMM + 1"))
	 1]

        ;; if x or y is in multiple cores.
        [else 
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display (format "COMM + ~a + ~a" x-comm y-comm)))
         (+ x-comm y-comm)]))

    (define (count-msg x-ast y-ast)
      (when debug
	    (pretty-display `(count-msg ,(send x-ast to-string) ,(send y-ast to-string))))
      (count-msg-place-type (get-field place-type x-ast) (get-field place-type y-ast)
			    x-ast y-ast))

    (define (count-msg-placeset p-ast placeset)
      ;(when debug (pretty-display `(count-msg-placeset)))
      (define p (get-field place-type p-ast))
      (define (loop placelist)
        (if (empty? placelist)
            0
            (let* ([others (cdr placelist)]
                 [me (car placelist)])
              (+ (loop others)
                 (if (andmap (lambda (x) (not (equal? x me))) others)
                     (count-msg-place-type p me p-ast)
                     0)))))
      (loop (set->list placeset)))

    (define/public (evaluate-comminfo func-ast)
      (let* ([name (get-field name func-ast)]
             [ret  (cdr (dict-ref env name))])
            (dict-set! env name 
                       (cons func-ast (comminfo (evaluate-with-sol (comminfo-msgs ret))
                                                (comminfo-placeset ret))))))
                 
    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__")))
    
    (define (place-at places index ast)
      (when (empty? places)
            (send ast index-out-of-bound index))
      (let* ([current (car places)]
             [from    (get-field from current)]
             [to      (get-field to current)])
        (if (and (>= index from) (< index to))
            (get-field place current)
            (place-at (cdr places) index ast))))
      
    (define/public (visit ast)
      (cond
       [(is-a? ast Const%)
          (raise "visitor-interpreter: can't visit Const%")
          (define place (find-place ast #f)) ; don't modify
	  (inc-space place est-num) ; increase space
          (comminfo 0 (set place))]


       [(is-a? ast Op%)
          (raise "visitor-interpreter: can't visit Op%")
          (define place (find-place ast #f)) ; don't modify
	  (inc-space-with-op place (get-field op ast)) ; increase space
	  (comminfo 0 (to-place-set place))
          ]

       [(is-a? ast Num%)
	  ;(define const (get-field n ast))
          (when debug (pretty-display (format ">> Num ~a" (send ast to-string))))

	  ;(send const accept this)
          (define place-type (find-place-type ast (get-field n ast)))
	  (set-field! place-type ast place-type)
	  (inc-space place-type est-num) ; increase space

          (comminfo 0 (to-place-set place-type))]
       
       [(is-a? ast Array%)
          ;; lookup place from env
          (define place-cluster (lookup env ast))
          (define places (car place-cluster))
          (define cluster (cdr place-cluster))
          (set-field! cluster ast cluster)

	  (define index (get-field index ast))

          (when debug (pretty-display (format ">> Array ~a" (send ast to-string))))

          (if (= (length places) 1)
              ;; Array lives in only one place
              (set-field! place-type ast (get-field place (car places)))
	      (if (is-a? index Num%)
		  ;; Know exact index
		  (set-field! place-type ast (place-at places (send index get-value) ast))
		  ;; Pair of list of possible places and index
		  (set-field! place-type ast (cons places index))))

          ;; Infer place
          (send index infer-place (get-field place-type ast))
	  (define index-ret (send index accept this))

          (inc-space places est-acc-arr) ; not accurate

          (when (and debug-sym (symbolic? (+ (comminfo-msgs index-ret) (count-msg index ast))))
                (pretty-display (format ">> SYM Array ~a\n~a" 
                                        (send ast to-string)
                                        (+ (comminfo-msgs index-ret) (count-msg index ast)))))

          (comminfo 
           (+ (comminfo-msgs index-ret) (count-msg index ast))
           (set-union (comminfo-placeset index-ret) (to-place-set places)))
          ]

       [(is-a? ast Var%)
        (when debug (pretty-display (format ">> Var ~a" (send ast to-string))))
            
        ;; if expend < expand then it is temp in temp = func()
        ;; we don't need to find place for such temp
        (if (<= (get-field expand ast) (get-field expect ast))
          ;; lookup place from env
          (let* ([place (lookup env ast)]
		 [inferred-place (get-field place-type ast)]
                 [place-type (if (and (at-any? place) inferred-place)
				 inferred-place
				 (to-place-type ast place))])
            ;; place can be list if var is iterator
            ;; need to call to-place-type to turn place-list into (place-list . index)
            (set-field! place-type ast place-type)
            (unless (is-a? ast Temp%)
              (inc-space place-type est-var))
            (comminfo 0 (to-place-set place)))
          (comminfo 0 (set)))
          ]

       [(is-a? ast UnaExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define op (get-field op ast))

          ;; set place-type
          (define place-type (find-place-type ast op))
          (set-field! place-type ast place-type)

          ;; Infer place-type
          (send e1 infer-place place-type)

          (define e1-ret (send e1 accept this))

	  (inc-space place-type (hash-ref space-map (get-field op op))) ; increase space

          (when debug
                (pretty-display (format ">> UnaOp ~a" (send ast to-string))))
          (when (and debug-sym (symbolic? (+ (comminfo-msgs e1-ret) (count-msg ast e1))))
                (pretty-display (format ">> SYM UnaOp ~a\n~a" (send ast to-string))
                                (+ (comminfo-msgs e1-ret) (count-msg ast e1))))
          
          (comminfo
           (+ (comminfo-msgs e1-ret) (count-msg ast e1))
           (set-union (comminfo-placeset e1-ret) (to-place-set place-type)))
          ]

       [(is-a? ast BinExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define op (get-field op ast))
          
          ;; set place-type
          (define place-type (find-place-type ast op))
	  (set-field! place-type ast place-type)

          ;; Infer place-type
          (send e1 infer-place place-type)
          (send e2 infer-place place-type)

          (define e1-ret (send e1 accept this))
          (define e2-ret (send e2 accept this))
          
	  (inc-space place-type (hash-ref space-map (get-field op op))) ; increase space

          (when debug
                (pretty-display (format ">> BinOp ~a ~a" (send ast to-string) place-type)))

          (when (and debug-sym
                 (symbolic? (+ (comminfo-msgs e1-ret) 
                               (comminfo-msgs e2-ret)
                               (count-msg ast e1)
                               (count-msg ast e2))))
                (pretty-display (format ">> SYM BinOp ~a\n~a" 
                                        (send ast to-string)
                                        (+ (comminfo-msgs e1-ret) 
                                           (comminfo-msgs e2-ret)
                                           (count-msg ast e1)
                                           (count-msg ast e2)))))


          (comminfo
           (+ (comminfo-msgs e1-ret) 
              (comminfo-msgs e2-ret)
              (count-msg ast e1)
              (count-msg ast e2))
           (set-union (set-union (comminfo-placeset e1-ret) (comminfo-placeset e2-ret)) (to-place-set place-type)))
          ]

       [(is-a? ast FuncCall%)
          (when debug
                (pretty-display (format ">> FuncCall ~a" (send ast to-string))))

	  (define func-ret (lookup env ast))
          (define func-ast (get-field signature ast))

          (define msgs (comminfo-msgs func-ret))
	  (define placeset (comminfo-placeset func-ret))

          ;; increase space
          (inc-space-placeset placeset est-funccall)

	  (for ([param (get-field stmts (get-field args func-ast))] ; signature
		[arg   (get-field args ast)]) ; actual
	       ;; infer place-type
	       (send arg infer-place (get-field place-type param))
	       (let ([arg-ret (send arg accept this)])
		 ;; infer place-type
		 (send param infer-place (get-field place-type arg))
		 (set! msgs (+ msgs (+ (count-msg param arg) (comminfo-msgs arg-ret))))
		 (set! placeset (set-union placeset (comminfo-placeset arg-ret)))))
          
	  ;; set place-type
	  (let ([return (get-field return func-ast)])
            ;; only set place-type when function returns non-expanded type
            ;; when it returns non-expanded type, temp = func(), 
            ;; and temp is already at the same place ast func()
            (when (is-a? return VarDecl%)
                  (set-field! place-type ast (get-field place return))))
		 
	  (comminfo msgs placeset)]

       [(or (is-a? ast TempDecl%)
	    (is-a? ast ReturnDecl%))
        (define type (get-field type ast))
        (define temp (car (get-field var-list ast)))
        (define place (find-place ast))
        (when debug
              (pretty-display (format ">> TempDecl ~a" temp)))

	(if (string? type)
	    (declare env temp place)
	    (let* ([entry (cdr type)]
		   [actual-type (car type)]
		   [place-expand (get-field place-list (get-field place ast))])
	      (for ([i (in-range entry)]
		    [p place-expand])
		   (declare env (ext-name temp i) p))))

	;; no inc-space & return empty
	(comminfo 0 (set))
	]
                
       [(is-a? ast VarDecl%) 
          (define place (find-place ast))
          (define var-list (get-field var-list ast))

	  ;; Param% only
	  (when (is-a? ast Param%)
		(set-field! place-type ast place))
          
          ;; put vars into env
          (for ([var var-list])
               (declare env var place))

          (when debug
                (pretty-display (format ">> VarDecl ~a ~a" var-list place)))

	  (unless (is-a? ast TempDecl%)
		  (inc-space place (* (length var-list) 
				      (if (is-a? ast Param%)
					  (add1 est-data)
					  est-data)))) ; increase space
          
	  (when debug
                (pretty-display (format ">> VarDecl ~a (after)" var-list)))
          
          (comminfo 0 (set place))
          ]

       [(is-a? ast ArrayDecl%)
          (define place-list (get-field place-list ast)) ; don't support place(x)
          (define cluster (get-field cluster ast))

          (when debug
                (pretty-display (format ">> ArrayDecl ~a" (get-field var ast))))

          ;; check boundaries
          (define last 0)

          (when (list? place-list)
                (for ([p place-list])
                     (let* ([from (get-field from p)]
                            [to   (get-field to p)])
                       (when (not (= from last))
                             (send ast bound-error))
                       (set! last to)
                       (inc-space (find-place p) (* (- to from) est-data)) ; increase space
                       ))

                (when (not (= (get-field bound ast) last))
                      (send ast bound-error)))
          
          ;; We might not need this. This is a dead code right now.
          (when (and (is-a? place-list Place%) 
                     (equal? (get-field at place-list) "any"))
            (raise-type-error (get-field var ast) "anything about @any" 
                              0 "Cannot assign @any for array."))
            

          ;; put array into env
          (declare env (get-field var ast) (cons place-list cluster))

          (comminfo 0 (to-place-set place-list))
          ]

       [(is-a? ast For%)
          (define place-list (get-field place-list ast)) ; don't allow place(x)

          (when debug
                (pretty-display (format ">> For ~a" (send (get-field iter ast) to-string))))

          ;; check boundaries
          (define last 0)
        
          (when (list? place-list)
              (for ([p place-list])
                   (let* ([from (get-field from p)]
                          [to   (get-field to p)])
                     (when (not (= from last))
                           (send ast bound-error))
                     (set! last to))))
		 ;(inc-space (get-field place p) est-for))) ; increase space

          ;; Add new scope for body.
          (push-scope)

          ;; Declare iterator
          (declare env 
                     (get-field name (get-field iter ast))
		     place-list)

          (define body-ret (send (get-field body ast) accept this))
          (define body-place-set (comminfo-placeset body-ret))
          (set-field! body-placeset ast body-place-set)
          
          ;(for ([p body-place-set])
          ;     (inc-space p est-for)) ; increase space (already accounts for i here)
          (inc-space-placeset body-place-set est-for)

          ;; Remove scope.
          (pop-scope)

          (comminfo
           (* (comminfo-msgs body-ret) (- (get-field to ast) (get-field from ast)))
           body-place-set)
          ]

       [(is-a? ast If%)
        (define condition (get-field condition ast))
        (define condition-ret (send condition accept this))

        (push-scope)
        (define true-ret (send (get-field true-block ast) accept this))
        (pop-scope)
        
        ;; between condition and true-block
        (define ret
          (comminfo
           (+ (comminfo-msgs condition-ret)
              (comminfo-msgs true-ret))
           (set-union (comminfo-placeset condition-ret) (comminfo-placeset true-ret))))

        ;; add to body-placeset
        (define body-placeset (comminfo-placeset true-ret))
        
        (define false-block (get-field false-block ast))
        (push-scope)
        (set! ret 
              (let ([false-block (get-field false-block ast)])
                (if false-block
                    ;; between condition and false-block
                    (let ([false-ret (send false-block accept this)])
                      ;; add to body-placeset
                      (set! body-placeset (set-union body-placeset 
                                                     (comminfo-placeset false-ret)))
                      (comminfo
                       (+ (comminfo-msgs ret)
                          (comminfo-msgs false-ret))
                       (set-union (comminfo-placeset ret) (comminfo-placeset false-ret))))
		    ret)))
        (pop-scope)

        ;; increase space
        (inc-space-placeset (comminfo-placeset ret) est-if)
        
        ;; save for use in flow and comminsert
        (set-field! body-placeset ast body-placeset)

	(when debug (pretty-display ">> FOR (count-msg-placeset)"))
        (comminfo (+ (comminfo-msgs ret) 
                     (count-msg-placeset condition body-placeset))
                  (comminfo-placeset ret))
        ]

       [(is-a? ast While%)
          (define condition (get-field condition ast))
          (define condition-ret (send condition accept this))
          
          (define body-ret (send (get-field body ast) accept this))
          
          ;; increase space
          (inc-space-placeset (comminfo-placeset body-ret) est-while)

          (set-field! body-placeset ast (comminfo-placeset body-ret))

          (comminfo
           (+ (* (get-field bound ast)
                 (+ (comminfo-msgs condition-ret)
                    (count-msg-placeset condition
                                        (comminfo-placeset body-ret))))
              (* (get-field bound ast) (comminfo-msgs body-ret)))
           (set-union (comminfo-placeset condition-ret) (comminfo-placeset body-ret)))
	  ]

       [(is-a? ast Assign%) 
          (when debug (newline))
          (define lhs (get-field lhs ast))
          (define rhs (get-field rhs ast))

          (when debug
                (pretty-display ">> Assign (lhs)"))
          ;; Visit lhs
          (define lhs-ret (send lhs accept this))

          (define lhs-place-type (get-field place-type lhs))
	  (define lhs-name (get-field name lhs))

          ;; infer type
          (send rhs infer-place lhs-place-type)

          ;; Visit rhs
          (define rhs-ret (send rhs accept this))
          (define rhs-place-type (get-field place-type rhs))

          ;; infer type
          (send lhs infer-place rhs-place-type)

	  ;; Don't increase space

          (define comm-lhs-rhs
            (if ;;(and (is-a? rhs FuncCall%) (regexp-match #rx"_temp" (get-field name lhs)))
                ;; always 0 when _tempX = func()
	        (get-field nocomm ast)
                0
                (count-msg lhs rhs)))
       
          (comminfo
           (+ (comminfo-msgs rhs-ret) (comminfo-msgs lhs-ret) comm-lhs-rhs)
           (set-union (comminfo-placeset rhs-ret) (comminfo-placeset lhs-ret)))
         ]

       [(is-a? ast Return%)
	(define val (get-field val ast))
	(if (list? val)
	    (for ([x val])
		 (send x accept this))
	    (send val accept this))
	(comminfo 0 (set))]

       [(is-a? ast Program%)

	(define ret #f)
	(for ([decl (get-field stmts ast)])
	     (let ([decl-ret (send decl accept this)])
	       (when (and (is-a? decl FuncDecl%) (equal? (get-field name decl) "main"))
		     (set! ret decl-ret))))
	;; Return main declaration
	ret]

       [(is-a? ast Block%) 
        
        (when debug
            (pretty-display ">> Block"))
        (let ([ret
               (foldl (lambda (stmt all) 
                        (let ([stmt-ret (send stmt accept this)])
                          (comminfo (+ (comminfo-msgs all) (comminfo-msgs stmt-ret))
                                    (set-union (comminfo-placeset all) 
                                               (comminfo-placeset stmt-ret)))))
                      (comminfo 0 (set)) (get-field stmts ast))])
          
          (when (and debug-sym (symbolic? (comminfo-msgs ret)))
                (pretty-display `(BLOCK SYM num-msgs = ,(comminfo-msgs ret))))
          ret
          )
          ]

       [(is-a? ast FuncDecl%)
          (push-scope)
	  (define return (get-field return ast))
          (define return-ret (send return accept this))
          (define args-ret (send (get-field args ast) accept this))
          (define body-ret (send (get-field body ast) accept this))
          (pop-scope)
          
          (when debug
                (pretty-display (format ">> FuncDecl ~a" (get-field name ast))))

          (define body-placeset (set-union (set-union (comminfo-placeset args-ret) (comminfo-placeset body-ret)) (comminfo-placeset return-ret)))
          (set-field! body-placeset ast body-placeset)

	  (let ([ret (comminfo (+ (+ (comminfo-msgs args-ret) (comminfo-msgs body-ret)) (comminfo-msgs return-ret))
			       body-placeset)])
	    ;; declare function
	    (declare env (get-field name ast) ret)
	    ret)]
       
       [(is-a? ast ConcreteFilterDecl%)
          (push-scope)
          (define input-ret (send (get-field input ast) accept this))
          (define output-ret (send (get-field output ast) accept this))
          (define args-ret (send (get-field args ast) accept this))
          (define body-ret (send (get-field body ast) accept this))
          (pop-scope)
          
          (when debug
            (pretty-display (format ">> ConcreteFilterDecl ~a" (get-field name ast))))
          
          (define body-placeset
            (set-union (set-union (set-union (comminfo-placeset args-ret)
                                             (comminfo-placeset body-ret))
                                  (comminfo-placeset input-ret))
                       (comminfo-placeset output-ret)))
          (set-field! body-placeset ast body-placeset)
          
          (let ([ret (comminfo (+ (+ (+ (comminfo-msgs args-ret)
                                        (comminfo-msgs body-ret))
                                     (comminfo-msgs input-ret))
                                  (comminfo-msgs output-ret))
                               body-placeset)])
            ;; declare function
            (declare env (get-field name ast) ret)
            ret)]
       
       [else (raise (format "visitor-interpreter: unimplemented for ~a" ast))]))
))
