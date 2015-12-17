#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "ast-util.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

(define placetype-linker%
  (class* object% (visitor<%>)
    (super-new)
    (define env (make-hash))
    (define param-decls (make-hash))
    (define actors* (make-hash))

    (define debug #f)
    
    ;; find actual place for @place(exp)
    (define (find-place ast)
      ;; (when (not (is-a? ast Livable%))
      ;;   (raise "find-place: ast is not Livable%"))
      
      (define place-exp (if (is-a? ast Exp%)
                            (get-field place-type ast)
                            (get-field place ast)))
      (when debug (pretty-display `(find-place ,ast ,place-exp)))

      (define (derive-place p)
        (when debug (pretty-display `(derive-place ,p)))
        (cond
         [(or (equal? p "any") (equal? p "io"))
          p]

         [(is-a? p Array%)
          (car (lookup env p))]

         [(is-a? p Var%) 
          (if (get-field sub p)
              (lookup-name env (ext-name (get-field name p) (get-field sub p)))
              (lookup env p))]
         
         [else
          (pretty-display "raise error")
          (raise (format "derive-place: unimplemented for ~a" p))]))

      (cond
        [(at-io? place-exp)
         (when debug (pretty-display `(at-io)))
         place-exp]
	[(at-any? place-exp)
	 place-exp]
        [(is-a? place-exp Place%)
         (when debug (pretty-display `(place%)))
         (let ([place (derive-place (get-field at place-exp))])
           place)]
        [else
         (when debug (pretty-display `(else)))
         place-exp]))
    
    ;; find index of @place(x[i]) if x is distributed.
    (define (find-index place)
      (when debug (pretty-display `(find-index ,place)))
      (if (is-a? place Place%)
          (let ([at (get-field at place)])
            (when debug (pretty-display `(find-index at ,at)))
            (if (string? at)
                at
                (let* ([index (if (is-a? at Array%)
                                 ;; return i when place = @place(x[i])
                                 (get-field index at)
                                 ;; return i when place = @place(i)
                                 at)]
                       [info (and (has-var? env (get-field name at)) (lookup env at))])
                  (when debug (pretty-display `(find-index at ,(send at to-string) 
                                                           info ,info)))
                  (when (and (pair? info) (not (list? info)) (cdr info)) 
                        ;; @place(x[i]) if x is cluster, illegal
                        (send place illegal-place))
                  (if (is-a? index Var%)
                      index
                      (raise "Place expression cannot be more complicated than @place(x[i])")))))
          ;; return false otherwise
          #f))

    ;; find place-type for @place(x)
    (define (find-place-type ast native)
      ;; (when (not (is-a? native Livable%))
      ;;   (raise "find-place-type: native is not Livable%"))
      
      (define place-type (get-field place-type ast))
      ;(pretty-display `(find-place-type ,place-type))
      (if (or (number? place-type) (pair? place-type))
          ; place-type is infered during abstract interpretation
          ; and the format is right, we can return right away
          place-type
          (begin
            ; get index for @place(i) before place because (find-place) will remove that info
            (define index (find-index (if (is-a? native Exp%)
                                          (get-field place-type native)
                                          (get-field place native))))
            (define place (find-place native))
	    ;(pretty-display `(find-place-type place ,place index ,index))
            (if (or (number? place) (place-type-dist? place) 
                    (at-any? place) (at-io? place))
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
    
    (define (place-at places index ast)
      (when (empty? places)
            (send ast index-out-of-bound index))
      (let* ([current (car places)]
             [from    (get-field from current)]
             [to      (get-field to current)])
        (if (and (>= index from) (< index to))
            (get-field place current)
            (place-at (cdr places) index ast))))
                   
    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__")))

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (when debug (pretty-display (format "PLACETYPE: Num ~a" (send ast to-string))))
        (set-field! place-type ast (find-place-type ast (get-field n ast)))]

       [(is-a? ast Array%)
        (when debug (pretty-display (format "PLACETYPE: Array ~a" (send ast to-string))))
        ;; lookup place from env
        (define ghost (get-field place-type ast))
        (when ghost
          (unless (is-a? (get-field at ghost) Array%)
            (raise (format "@place for ghost region of ~a has to be an array. Error at line ~a."
			   (send ast to-string) (send ast get-line)))))
        (define delegate (if ghost (get-field at ghost) ast))
        (define place-cluster (lookup env delegate))
        (define places (car place-cluster))
        (define cluster (cdr place-cluster))
        (set-field! cluster ast cluster)
        
        (send (get-field index ast) accept this)
        (define index (get-field index delegate))
        
        (when debug (pretty-display (format ">> Array ~a, cluster = ~a" 
					    (send ast to-string) cluster)))
        
        (if (= (length places) 1)
            ;; Array lives in only one place
            (set-field! place-type ast (get-field place (car places)))
            (if (is-a? index Num%)
                ;; Know exact index
                (set-field! place-type ast (place-at places (send index get-value) ast))
                ;; Pair of list of possible places and index
                (set-field! place-type ast (cons places index))))
        
        ;; Infer place
        ;(send index infer-place (get-field place-type ast))
        ]

       [(is-a? ast Var%)
        (when debug (pretty-display (format "PLACETYPE: Var ~a" (send ast to-string))))
        (define place (lookup env ast))
        (define inferred-place (get-field place-type ast))
        (define place-type (if (and (at-any? place) inferred-place)
                               inferred-place
                               (to-place-type ast place)))
        (set-field! place-type ast place-type)

        ;; if expect > expand then it is temp in temp = func()
        ;; we don't need to find place for such temp
        ;; (if (<= (get-field expand ast) (get-field expect ast))
        ;;   ;; lookup place from env
        ;;   (let* ([place (lookup env ast)]
	;; 	 [inferred-place (get-field place-type ast)]
        ;;          [place-type (if (and (at-any? place) inferred-place)
	;; 			 inferred-place
	;; 			 (to-place-type ast place))])
        ;;     ;; place can be list if var is iterator
        ;;     ;; need to call to-place-type to turn place-list into (place-list . index)
        ;;     (set-field! place-type ast place-type)
        ;;     (comminfo 0 (to-place-set place)))
        ;;   (comminfo 0 (set)))
        ]

       [(is-a? ast UnaExp%)
        (when debug (pretty-display (format "PLACETYPE: UnaExp ~a" (send ast to-string))))
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        
        (send e1 accept this)
        
        ;; set place-type
        (define place-type (find-place-type ast op))
        (when (and (symbolic? place-type)
                   (at-any? (get-field place-type e1)))
              (set-field! place op (new Place% [at "any"]))
              (set! place-type (new Place% [at "any"])))
        (set-field! place-type ast place-type)
       ]

       [(is-a? ast BinExp%)
        (when debug (pretty-display (format "PLACETYPE: BinExp ~a" (send ast to-string))))
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        
        (send e1 accept this)
        (send e2 accept this)
        
        ;; set place-type
        (define place-type (find-place-type ast op))
        (when (and (symbolic? place-type)
                   (at-any? (get-field place-type e1))
                   (at-any? (get-field place-type e2)))
              (set-field! place op (new Place% [at "any"]))
              (set! place-type (new Place% [at "any"])))
        (set-field! place-type ast place-type)
        ]

       [(is-a? ast FuncCall%)
        (when debug (pretty-display (format "PLACETYPE: FuncCall ~a" (send ast to-string))))
        ;; infer place-type
        ;; (for ([param (get-field stmts (get-field args (get-field signature ast)))]
        ;;       [arg (flatten-arg (get-field args ast))])
        ;;      (send arg infer-place (get-field place-type param))
        ;;      )
        
        ;; visit children
        (for ([arg (get-field args ast)])
             (send arg accept this))]

       [(or (is-a? ast TempDecl%)
	    (is-a? ast ReturnDecl%))
        (when debug (pretty-display (format "PLACETYPE: TempDecl")))
        (define type (get-field type ast))
        (define temp (car (get-field var-list ast)))
        (define place (find-place ast))

	(if (pair? type)
	    (let* ([entry (cdr type)]
		   [actual-type (car type)]
		   [place-expand (get-field place-list (get-field place ast))])
	      (for ([i (in-range entry)]
		    [p place-expand])
		   (declare env (ext-name temp i) p)))
            (declare env temp place))
        ]

       [(is-a? ast VarDecl%) 
        (define var-list (get-field var-list ast))
        (define place (find-place ast))
        (when debug (pretty-display (format "PLACETYPE: VarDecl ~a, place=~a" var-list place)))
        (when (is-a? (get-field place ast) Place%)
              (send ast save-org-place))
	(set-field! place ast place) ;; TODO: check
        
        ;; Param% only
        (when (is-a? ast Param%)
              (set-field! place-type ast place)
              (hash-set! param-decls (car (get-field var-list ast)) ast)
              )
        
        ;; put vars into env
        (for ([var var-list])
             (declare env var place))]

       [(is-a? ast Assume%)
        (define exp (get-field e1 ast))
        (unless (and (is-a? exp BinExp%) 
                     (is-a? (get-field e1 exp) Var%)
                     (is-a? (get-field e2 exp) Num%)
                     (member (get-field op (get-field op exp)) (list "<=" "=")))
                (raise (format "Assume only supports expression in this form assume(var op n) where op can either be <= or =. Error at line ~a." (send ast get-line))))

        (define e1 (get-field e1 exp))
        (unless (hash-has-key? param-decls (send e1 to-string))
                (raise (format "Variable inside assume has to be function parameter. Error at line ~a." (send ast get-line))))

        (define param (hash-ref param-decls (send e1 to-string)))
        (set-field! assume param exp)]

       [(is-a? ast ArrayDecl%)
        (when debug (pretty-display (format "PLACETYPE: ArrayDecl")))
        (define place-list (get-field place-list ast)) ; don't support place(x)
	(when (or (number? place-list)
		  (and (list? place-list) (= (length place-list) 1)))
	      (set-field! cluster ast #f))
        (define cluster (get-field cluster ast))

        (for ([p place-list])
             (when (is-a? (get-field place p) Place%)
                   (send p save-org-place))
             (set-field! place p (find-place p)))
          
        ;; We might not need this. This is a dead code right now.
        ;; (when (and (is-a? place-list Place%) 
        ;;            (equal? (get-field at place-list) "any"))
        ;;       (raise-type-error (get-field var ast) "anything about @any" 
        ;;                         0 "Cannot assign @any for array."))

        ;; put array into env
        (declare env (get-field var ast) (cons place-list cluster))
        ]

       [(is-a? ast For%)
        (when debug (pretty-display (format "PLACETYPE: For")))
        
        ;; Add new scope for body.
        (push-scope)
        
        ;; Declare iterator
        (declare env 
                 (get-field name (get-field iter ast))
                 (get-field place-list ast))
        (send (get-field body ast) accept this)

        ;; Remove scope.
        (pop-scope)]

       [(is-a? ast If%)
        (when debug (pretty-display (format "PLACETYPE: If")))
        (send (get-field condition ast) accept this)
        (push-scope)
        (send (get-field true-block ast) accept this)
        (pop-scope)
        (define false-block (get-field false-block ast))
        (when false-block
              (push-scope)
              (send false-block accept this)
              (pop-scope))]

       [(is-a? ast While%)
        (when debug (pretty-display (format "PLACETYPE: While")))
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (push-scope)
        (send (get-field body ast) accept this)
        (pop-scope)]

       [(is-a? ast AssignTemp%)
        (when debug (pretty-display (format "PLACETYPE: AssignTemp")))
        (send (get-field rhs ast) accept this)]

       
       [(is-a? ast Assign%) 
        (when debug (pretty-display (format "PLACETYPE: Assign")))
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))

        (send lhs accept this)
        ;(define lhs-place-type (get-field place-type lhs))
        ;(send rhs infer-place lhs-place-type)
        (send rhs accept this)
        ;(define rhs-place-type (get-field place-type rhs))
        ;(send lhs infer-place rhs-place-type)
        ]

       [(is-a? ast Return%)
        (when debug (pretty-display (format "PLACETYPE: Return")))
	(define val (get-field val ast))
	(if (list? val)
	    (for ([x val])
		 (send x accept this))
	    (send val accept this))]

       [(is-a? ast FuncDecl%)
        (when debug (pretty-display (format "\nPLACETYPE: FuncDecl ~a" (get-field name ast))))
        (push-scope)
        (define args (get-field args ast))

        ;; If this function is actor*, then make sure all its paramers are at the same place.
        (when (hash-has-key? actors* (get-field name ast))
              (define args-list (get-field stmts args))
              (when (> (length args-list) 1)
                    (define place (get-field place (car args-list)))
                    (define place-type (get-field place-type (car args-list)))
                    (for ([arg (drop args-list 1)])
                         (set-field! place arg place)
                         (set-field! place-type arg place-type))))
        
        (send args accept this)
        (when (get-field precond ast)
              (send (get-field precond ast) accept this))
        (when (get-field return ast)
              (send (get-field return ast) accept this))
        (send (get-field body ast) accept this)
        (pop-scope)]

       [(is-a? ast Block%)
        (when (is-a? ast Program%)
              (set! actors* (get-field actors* ast)))
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]
		
       [else (raise (format "visitor-placetype: unimplemented for ~a" ast))]))
    ))

       
