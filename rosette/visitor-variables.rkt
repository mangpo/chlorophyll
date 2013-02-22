#lang s-exp rosette

(require "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define var-collector%
  (class* object% (visitor<%>)
    (super-new)
    
    (define/public (visit ast)
      (cond
        [(or (or (is-a? ast Num%) 
                 (is-a? ast Op%))
             (is-a? ast VarDecl%))
         (let ([place (get-field place ast)])
           (if (symbolic? place)
               (set place)
               (set)))
         ]
      
        [(is-a? ast Var%)
         (set) ; we handle vat at declaration.
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