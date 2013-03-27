#lang s-exp rosette

(require "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define printer%
  (class* object% (visitor<%>)
    (super-new)
    (define indent 0)
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (display (format "~a@~a ~a;"
                               (get-field type ast)
                               (send ast get-place)
                               (get-field var-list ast)))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a@{~a} ~a;"
                               (get-field type ast)
                               (send ast place-to-string)
                               (get-field var ast)))
         ]
        
        [(is-a? ast Num%)
         (display (format "~a@~a "
                        (get-field n ast)
                        (send ast get-place)))
         ]
        
        [(is-a? ast Op%)
         (display (format "~a@~a "
                        (get-field op ast)
                        (send ast get-place)))
         ]
      
        [(is-a? ast Var%)
         (display (format "~a "
                        (get-field name ast)))
         ]
        
        [(is-a? ast UnaExp%)
         (send (get-field op ast) accept this)
         (send (get-field e1 ast) accept this)
         ]
        
        [(is-a? ast BinExp%)
         (display "(")
         (send (get-field e1 ast) accept this)
         (send (get-field op ast) accept this)
         (send (get-field e2 ast) accept this)
         (display ")")
         ]
        
        [(is-a? ast Assign%)
         (send (get-field lhs ast) accept this)
         (display "= ")
         (send (get-field rhs ast) accept this)
         ]
        
        [(is-a? ast Block%)
         (for/list ([stmt (get-field stmts ast)])
           (send stmt accept this)
           (newline))]
        
        [else (raise "Error: printer unimplemented!")]
        
        ))))