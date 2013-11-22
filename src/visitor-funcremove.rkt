#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define func-remover%
  (class* object% (visitor<%>)
    (super-new)
    (define functions (set "main"))

    (define/public (visit ast)
      (cond
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
	(send (get-field body ast) accept this)]
       
       [(is-a? ast If%)
	(send (get-field condition ast) accept this)
	(send (get-field true-block ast) accept this)
	(when (get-field false-block ast)
	      (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
	(send (get-field body ast) acccept this)]

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
	      (set-field! stmts ast (filter (lambda (x) (set-member? functions (get-field name x)))
					    (get-field stmts ast))))
	]

       [else void]))))
