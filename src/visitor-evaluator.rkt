#lang s-exp rosette

(require "header.rkt" "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define symbolic-evaluator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field num-cores)
    
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
        (when (at-io? (get-field place ast))
              (set-field! place ast (sub1 num-cores))
              (when (is-a? ast Param%)
                    (set-field! place-type ast (sub1 num-cores))))
        ]

       [(is-a? ast LivableGroup%)
        ;; (let ([place (get-field place-list ast)])
        ;;   (if (list? place)
        ;;       (for ([p place])
        ;;            (send p accept this))
        ;;       ;(set-field! place ast (evaluate place global-sol))))
        ;;       (send ast to-concrete)))
	(send ast to-concrete)
        ]

       [(is-a? ast For%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p place])
                   (send p accept this))
              ;(set-field! place ast (evaluate place global-sol))))
              (send ast to-concrete)))

        (send (get-field body ast) accept this)
        (evaluate-placeset)]

       [(is-a? ast Num%)
        (send ast to-concrete)
        ]

       [(is-a? ast Array%)
        (send ast to-concrete)
        (send (get-field index ast) accept this)
        (send (get-field index ast) infer-place (get-field place-type ast))]

       [(or (is-a? ast Num%)
            (is-a? ast Var%) 
            (is-a? ast ProxyReturn%))
        (send ast to-concrete)]

       [(is-a? ast UnaExp%)
        (send (get-field op ast) accept this)
        (send (get-field e1 ast) accept this)
        (send ast to-concrete)
        (send ast infer-place (get-field place-type ast))
        ]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e1 ast))
        (send (get-field op ast) accept this)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (send ast to-concrete)
        (send ast infer-place (get-field place-type ast))
        ]

       [(is-a? ast FuncCall%)
	(define func-ast (get-field signature ast))
        (define params (get-field stmts (get-field args func-ast)))
	(define name (get-field name ast))
        (for ([arg (flatten-arg (get-field args ast))])
	     (send arg accept this))
        (send ast to-concrete)

        ;; convert io                      
        (when (at-io? (get-field place-type ast))
              (set-field! place-type ast (sub1 num-cores)))
        
        ;; (for ([param params])
        ;;      (when (at-io? (get-field place-type param))
        ;;            (set-field! place param (sub1 num-cores))
        ;;            (set-field! place-type param (sub1 num-cores))))
        (when (or (equal? name "in") (equal? name "out"))
              (send func-ast accept this))

	;; infer
	(for ([param params] ; signature
              [arg   (flatten-arg (get-field args ast))]) ; actual
          (send arg infer-place (get-field place-type param))
          ;(send param infer-place (get-field place-type arg))
          )
	
        ;; return can't be at any, so we don't need to infer return
        ]

       [(is-a? ast Assign%)
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))
        (send lhs accept this)
        (send rhs accept this)
        (send lhs infer-place (get-field place-type rhs))
        (send rhs infer-place (get-field place-type lhs))
        ]

       [(is-a? ast Return%)
	(define val (get-field val ast))
        (if (list? val)
	    (for ([x val])
		 (send x accept this))
	    (send val accept this))]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (let ([false-block (get-field false-block ast)])
          (when false-block
              (send false-block accept this)))
        (evaluate-placeset)
        ]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        (evaluate-placeset)
        ]

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (when (get-field return ast)
              (send (get-field return ast) accept this))
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)
        (evaluate-placeset)]

       [else (raise (format "Error: symbolic-evaluator unimplemented for ~a!" ast))]

       ))))
