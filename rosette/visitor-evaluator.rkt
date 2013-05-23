#lang s-exp rosette

(require "header.rkt" "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define symbolic-evaluator%
  (class* object% (visitor<%>)
    (super-new)
    
    (define/public (visit ast)

      (define (evaluate-placeset)
        (let ([symplace-list (set->list (get-field body-placeset ast))])
          (set-field! body-placeset ast
                      (list->set 
                       (map (lambda (x) (evaluate-with-sol x)) 
                            symplace-list)))))
        
      (cond
       [(is-a? ast Livable%)
        ;(set-field! place ast (send ast get-place))
        (send ast to-concrete)
        ]

       [(is-a? ast LivableGroup%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p place])
                   (send p accept this))
              ;(set-field! place ast (evaluate place global-sol))))
              (send ast to-concrete)))

        (when (is-a? ast For%)
              (send (get-field body ast) accept this)
              (evaluate-placeset))
        ]

       [(is-a? ast Num%)
	 (send (get-field n ast) accept this)
         (send ast to-concrete)
         ]

       [(is-a? ast Array%)
        (send ast to-concrete)
        (send (get-field index ast) accept this)]

       [(is-a? ast Var%) 
        (send ast to-concrete)]

       [(is-a? ast UnaExp%)
        (send (get-field op ast) accept this)
        (send (get-field e1 ast) accept this)
        (send ast to-concrete)
        ]

       [(is-a? ast BinExp%)
        (send (get-field op ast) accept this)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (send ast to-concrete)
        ]

       [(is-a? ast FuncCall%)
        (for ([arg (get-field args ast)])
             (send arg accept this))
        (send ast to-concrete)]

       [(is-a? ast Assign%)
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (let ([false-block (get-field false-block ast)])
          (when false-block
              (send false-block accept this)))
        (evaluate-placeset)
        ]

       [(is-a? ast While%)
        (send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        (evaluate-placeset)
        ]

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (send (get-field return ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)
        (evaluate-placeset)]

       [(is-a? ast Program%)
        (for ([decl (get-field decls ast)])
             (send decl accept this))]

       [else (raise (format "Error: symbolic-evaluator unimplemented for ~a!" ast))]

       ))))