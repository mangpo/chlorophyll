#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt"
         "visitor-expr-interpreter.rkt")

(provide (all-defined-out))

(define post-unroller%
  (class* object% (visitor<%>)
    (super-new)
    (define functions (set "main"))
    (define arrays (make-hash))
    (define env (make-hash))

    (define debug #t)

    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__")))

    (define/public (visit ast)
      (cond
       [(is-a? ast ArrayDecl%)
        (when debug
              (pretty-display (format "POSTUNROLL: ArrayDecl ~a" (get-field var ast))))
        ;; only construct ghost region when it is not initialized.
        (when (and (get-field ghost ast) (empty? (get-field ghost ast)))
              (when debug (pretty-display "DECLARE!"))
              (hash-set! arrays (get-field var ast) ast))]

       [(is-a? ast Array%)
        (when debug
              (pretty-display (format "POSTUNROLL: Array ~a" (send ast to-string))))
        (when (and (get-field ghost ast) (hash-has-key? arrays (get-field name ast)))
              (when debug (pretty-display "GHOST!"))
              (define range (send (get-field index ast) 
                                  accept (new expr-interpreter% [env env])))
              (define decl (hash-ref arrays (get-field name ast)))
              (send decl add-ghost-region range (get-field place-type ast)))]

       [(is-a? ast UnaExp%)
	(send (get-field e1 ast) accept this)]

       [(is-a? ast BinExp%)
	(send (get-field e1 ast) accept this)
	(send (get-field e2 ast) accept this)]

       [(is-a? ast FuncCall%)
	(set! functions (set-add functions (get-field name ast)))
	(for ([arg (get-field args ast)])
	     (send arg accept this))]

       [(is-a? ast For%)
        (push-scope)
        (declare env 
                 (get-field name (get-field iter ast)) 
                 (cons (get-field from ast) (sub1 (get-field to ast))))
	(send (get-field body ast) accept this)
        (pop-scope)
        ]
       
       [(is-a? ast If%)
	(send (get-field condition ast) accept this)
	(send (get-field true-block ast) accept this)
	(when (get-field false-block ast)
	      (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
	(send (get-field body ast) accept this)]

       [(is-a? ast Assign%)
	(send (get-field lhs ast) accept this)
	(send (get-field rhs ast) accept this)]

       [(is-a? ast Return%)
	(define val (get-field val ast))
	(if (list? val)
	    (for ([v val])
		 (send v accept this))
	    (send val accept this))]

       [(is-a? ast FuncDecl%)
	(send (get-field body ast) accept this)]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))

	(when (is-a? ast Program%)
	      (set-field!
               stmts ast
               (filter (lambda (x)
                         (or (not (is-a? x FuncDecl%))
                             (set-member? functions (get-field name x))))
                       (get-field stmts ast))))
	]

       [else void]))))
