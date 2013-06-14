#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "visitor-interface.rkt" )

(provide (all-defined-out))

(define place-collector%
  (class* object% (visitor<%>)
    (super-new)
    (init-field collect?)

    (define (create-set place)
      (if (collect? place)
          (set place)
          (set)))

    (define (place-set place)
      (cond
       [(list? place)
	(foldl (lambda (p var-set) (set-union var-set (send p accept this)))
	       (set) place)]

       [(pair? place)
	(place-set (car place))]
       
       [(is-a? place TypeExpansion%)
	(foldl (lambda (x placeset) (set-union placeset (place-set x)))
	       (set)
	       (get-field place-list place))]

       [else (create-set place)]))
    
    (define/public (visit ast)
      
      (define-syntax-rule (union-set-from-list x)
	(foldl (lambda (ele var-set) (set-union var-set (send ele accept this)))
                (set) x))

      (cond
        [(is-a? ast Livable%)
         (place-set (get-field place ast))
         ]

        [(is-a? ast LivableGroup%)
	 (place-set (get-field place-list ast))
	 ]

        [(is-a? ast For%)
         (let ([ret (place-set (get-field place-list ast))])
           (set-union ret (send (get-field body ast) accept this)))]

	[(is-a? ast Num%)
	 (send (get-field n ast) accept this)
	 ]
      
        [(is-a? ast Var%) 
	 (set) ; we handle var at declaration. 
	 ]
        
        [(is-a? ast UnaExp%)
         (set-union (send (get-field op ast) accept this)
                    (send (get-field e1 ast) accept this))
         ]
        
        [(is-a? ast BinExp%)
         (set-union (send (get-field op ast) accept this)
                    (send (get-field e1 ast) accept this)
                    (send (get-field e2 ast) accept this))
         ]
        
        [(is-a? ast Assign%)
         (send (get-field rhs ast) accept this)
         ]

	[(is-a? ast FuncCall%)
	 (union-set-from-list (get-field args ast))]

        [(is-a? ast If%)
         (set-union (send (get-field condition ast) accept this)
                    (send (get-field true-block ast) accept this)
                    (let ([false-block (get-field false-block ast)])
                      (if false-block
                          (send false-block accept this)
                          (set))))
         ]

	[(is-a? ast While%)
	 (set-union (send (get-field condition ast) accept this)
		    (send (get-field body ast) accept this))]
        
        [(is-a? ast Block%)
	 (union-set-from-list (get-field stmts ast))]

        [(is-a? ast FuncDecl%)
         (let ([return-set (send (get-field return ast) accept this)]
               [args-set (send (get-field args ast) accept this)]
               [body-set (send (get-field body ast) accept this)])
           (set-union (set-union return-set args-set) body-set))]
        
        [else (raise (format "Error: var-collector unimplemented for ~a" ast))]
	))))
        
        
