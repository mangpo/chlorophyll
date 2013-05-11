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
    
    (define/public (visit ast)
      
      (define-syntax-rule (union-set-from-list x)
	(foldl (lambda (ele var-set) (set-union var-set (send ele accept this)))
                (set) x))

      (cond
        [(is-a? ast Livable%)
         (let ([place (get-field place ast)])
           (create-set place))
         ]

        [(is-a? ast LivableGroup%)
	 (let* ([place (get-field place-list ast)]
                [ret (if (list? place)
                         (foldl (lambda (p var-set) (set-union var-set (send p accept this)))
                                (set) place)
                         (create-set place))])
           (if (is-a? ast For%)
	       (set-union ret (send (get-field body ast) accept this))
	       ret))
	 ]

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
         (set-union (set-union (send (get-field op ast) accept this)
                               (send (get-field e1 ast) accept this))
                    (send (get-field e2 ast) accept this))
         ]
        
        [(is-a? ast Assign%)
         (send (get-field rhs ast) accept this)
         ]

	[(is-a? ast FuncCall%)
	 (union-set-from-list (get-field args ast))]

        [(is-a? ast If%)
         (set-union (set-union (send (get-field condition ast) accept this)
                               (send (get-field true-block ast) accept this))
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

        [(is-a? ast Program%)
	 (union-set-from-list (get-field decls ast))]
        
        [else (raise "Error: var-collector unimplemented!")]
	))))
        
        
