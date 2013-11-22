#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-cloner.rkt")

(provide (all-defined-out))

;; Unroll for loop according to array distributions of variables inside its body.
;; The sub AST inside unrolled for loop is duplicated.
;; This visitor mutates the AST.
(define loop-unroller%
  (class* object% (visitor<%>)
    (super-new)
    (define index-map (make-hash))
    (define cloner #f)
    (define debug #f)

    (define/public (visit ast)
      (cond
       [(is-a? ast VarDeclDup%)
        (pretty-display (format "UNROLL: VarDeclDup ~a unroll = ~a" 
                                (get-field var-list ast)
                                (get-field unroll ast)))
        (for/list ([i (in-range (get-field unroll ast))])
                  (send cloner set-id i)
                  (send ast accept cloner))
        ]

       [(is-a? ast AssignDup%)
        (pretty-display (format "UNROLL: AssignDup unroll = ~a" 
                                (get-field unroll ast)))
        (for/list ([i (in-range (get-field unroll ast))])
                  (send cloner set-id i)
                  (send ast accept cloner))
        ]

       [(is-a? ast For%)
        (pretty-display "UNROLL: For")
        (define body (send (get-field body ast) accept this))
        (define iter (get-field iter ast))
        (define iter-name (get-field name iter))
        (define ranges (get-field unroll ast))
        (pretty-display ranges)

        (if ranges
            (for/list ([range ranges]
                       [id (in-range (length ranges))])
              (pretty-display "UNROLL: For (2)")
              (send cloner set-range (car range) (cdr range) iter-name id)
              (pretty-display "UNROLL: For (3)")
              (new For% 
                   [iter (send iter clone)] 
                   [body (send body accept cloner)]
                   [from (car range)]
                   [to (add1 (cdr range))]
                   [known (get-field known ast)]
                   [place-list #f]
                   [body-placeset (get-field body-placeset ast)]))
            ast)
        ;; Return list of For%
        ]

       [(is-a? ast If%)
        ;(pretty-display "UNROLL: If")
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
        ast]

       [(is-a? ast While%)
        (send (get-field body ast) accept this)
        ast]


       [(is-a? ast Program%)
        (set! cloner (new range-cloner% [program ast]))
        (for ([decl (get-field stmts ast)])
             (send decl accept this))]

       [(is-a? ast Block%)
        (set-field! stmts ast
                    (flatten (map (lambda (x) (send x accept this)) 
                                  (get-field stmts ast))))
        ;(pretty-display "UNROLL: Block (after)")
        ;(send ast pretty-print)
        ast
        ]

       [(is-a? ast FuncDecl%)
        (send cloner add-function ast)
        (send (get-field body ast) accept this)]

       [else ast]

       ))))
       
            
