#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-cloner.rkt"
         "visitor-placeset.rkt")

(provide (all-defined-out))

;; Unroll for loop according to array distributions of variables inside its body.
;; The sub AST inside unrolled for loop is duplicated.
;; This visitor mutates the AST.
(define loop-unroller%
  (class* object% (visitor<%>)
    (super-new)
    (init-field program)
    (define index-map (make-hash))
    (define cloner (new range-cloner%))
    (define placeset-collector
      (new placeset-collector% [save #f] [actors (get-field actors program)]))

    (define debug #f)

    (define/public (visit ast)
      (cond
       [(is-a? ast VarDeclDup%)
        (when debug (pretty-display (format "UNROLL: VarDeclDup ~a" 
					    (get-field var-list ast))))
        (define n (length (get-field unroll (get-field loop ast))))
        (when debug (pretty-display (format "UNROLL: VarDeclDup ~a unroll = ~a" 
					    (get-field var-list ast) n)))
        
        (define conflicts (list))

        (define ret
          (for/list ([i (in-range n)])
                    (send cloner set-id i)
                    (let ([new-vardecl (send ast accept cloner)])
                      (set! conflicts (cons (send new-vardecl accept placeset-collector)
                                            conflicts))
                      new-vardecl)))

        (set-field! conflict-list program
                    (cons conflicts (get-field conflict-list program)))
        ret
        ]

       [(is-a? ast For%)
        (when debug (pretty-display "UNROLL: For"))
        (define body (send (get-field body ast) accept this))
        (define iter (get-field iter ast))
        (define iter-name (get-field name iter))
        (define ranges (get-field unroll ast))
        (when debug (pretty-display ranges))

        ;; prepare to cllect newly created bodies from map-reduce construct
        (set-field! new-funcs cloner (list))

        (define ret
          (if ranges
              (for/list ([range ranges]
                         [id (in-range (length ranges))])
                        (when debug (pretty-display (format "UNROLL: For (2) ~a" range)))
                        (send cloner set-range (car range) (cdr range) iter-name id)
                        (when debug (pretty-display (format "UNROLL: For (3) ~a" range)))
                        (new For% 
                             [iter (send iter clone)] 
                             [body (send body accept cloner)]
                             [from (car range)]
                             [to (add1 (cdr range))]
                             [known (get-field known ast)]
                             [place-list #f]
                             [body-placeset (get-field body-placeset ast)]))
              ast))
        (when debug (pretty-display "UNROLL: For (DONE)"))
        
        ;; add newly created functions from reduce to program AST
	(define new-funcs (get-field new-funcs cloner))
        (set-field! stmts program (append new-funcs
                                          (get-field stmts program)))

        (when debug (pretty-display (format "UNROLL: For unroll, new-funcs = ~a" new-funcs)))

        ;; add to conflict-list
	(when (and (is-a? ast ParFor%) ranges)
	      (send placeset-collector set-functions new-funcs)
	      (set-field! conflict-list program
			  (cons (for/list ([x ret])
					  (send (get-field body x) accept placeset-collector))
				(get-field conflict-list program))))
        
        ret
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


       [(is-a? ast BlockDup%)
        (define n (length (get-field unroll (get-field loop ast))))
        (when debug (pretty-display (format "UNROLL: BlockDup unroll = ~a" n)))
        (set-field! stmts ast
                    (flatten (map (lambda (x) (send x accept this)) 
                                  (get-field stmts ast))))

        (set-field! stmts ast
                    (flatten
                     (for/list ([i (in-range n)])
                               (send cloner set-id i)
                               (let ([new-block (send ast accept cloner)])
                                 (get-field stmts new-block)))))

        ast
        ]

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
       
            
