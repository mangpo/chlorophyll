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
    (init-field [index-map (make-hash)])
    (define debug #f)

    (define/public (visit ast)

      (cond

       [(or (is-a? ast FuncCall%)
            (is-a? ast VarDecl%) 
            (is-a? ast ArrayDecl%)
            (is-a? ast Assign%)
            (is-a? ast Return%))
        ast]

       [(is-a? ast For%)
        (pretty-display "UNROLL: For")
        (define body (send (get-field body ast) accept this))
        (define iter (get-field iter ast))
        (define iter-name (get-field name iter))
        (define ranges (get-field unroll ast))
        (pretty-display ranges)

        (if ranges
            (for/list ([range ranges])
                      (pretty-display "UNROLL: For (2)")
                      (define body-clone (send body accept (new range-cloner% 
                                                                [from (car range)]
                                                                [to (cdr range)]
                                                                [index iter-name])))
                      (pretty-display "UNROLL: For (3)")
                      (new For% 
                           [iter (send iter clone)] 
                           [body body-clone]
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
        (send (get-field body ast) accept this)]

       ))))
       
            
