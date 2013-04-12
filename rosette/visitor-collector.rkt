#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "visitor-interface.rkt" )

(provide (all-defined-out))

(define place-collector%
  (class* object% (visitor<%>)
    (super-new)
    (init-field collect?)
    
    (define/public (visit ast)
      (cond
        [(is-a? ast Livable%)
         (let ([place (get-field place ast)])
           (if (collect? place)
               (set place)
               (set)))
         ]

        [(is-a? ast LivableGroup%)
	 (let ([ret (foldl (lambda (p var-set) (set-union var-set (send p accept this)))
			   (set) (get-field place-list ast))])
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
        
        [(is-a? ast Block%)
         (foldl (lambda (stmt var-set) (set-union var-set (send stmt accept this)))
                (set) (get-field stmts ast))
         ]
        
        [else (raise "Error: var-collector unimplemented!")]
        
        ))))
