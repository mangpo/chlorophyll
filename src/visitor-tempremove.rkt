#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(provide (all-defined-out))

(define temp-remover%
  (class* object% (visitor<%>)
    (super-new)
    (define removed (list))
    (define debug #f)

    (define/public (visit ast)
      (cond
       [(is-a? ast If%)
	(send (get-field true-block ast) accept this)
	(when (get-field false-block ast)
	      (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
	(send (get-field pre ast) accept this)
	(send (get-field body ast) accept this)]

       [(is-a? ast For%)
	(send (get-field body ast) accept this)]

       [(is-a? ast Block%)
	(define (remove-temp lst)
	  ;(pretty-display `(remove-temp ,lst))
	  (if (empty? lst)
	      lst
	      (let* ([rest (remove-temp (cdr lst))]
		     [me (car lst)]
		     [next (and (not (empty? rest)) (car rest))])
		(if (and next (is-a? me Assign%) (is-a? next Assign%))
		    (let ([def (get-field lhs me)]
			  [use (get-field rhs next)])
		      (begin
                        (when debug
                              (pretty-display `(remove-temp ,(send def to-string) ,(send use to-string))))
		      (if (and (is-a? def Temp%) (is-a? use Temp%)
			       (equal? (get-field name def) (get-field name use))
			       (equal? (get-field sub def) (get-field sub use)))
			  (begin
                            (when debug (pretty-display "REMOVE!"))
                            (set! removed (cons (get-field name def) removed))
			    (cons (new Assign% [lhs (get-field lhs next)] [rhs (get-field rhs me)])
				  (cdr rest)))
			  (cons me rest))))
		    (cons me rest)))))

	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))
	(set-field! stmts ast (remove-temp (get-field stmts ast)))]

       [(is-a? ast FuncDecl%)
	(set! removed (list))
        (define body (get-field body ast))
        ;; Remove unnecesssy temps in the body.
	(send (get-field body ast) accept this)
        ;; Remove declarations associated to those temps.
        (set-field! stmts body
                    (filter (lambda (x) 
                              (not (and (is-a? x VarDecl%)
                                        (not (pair? (get-field type x))) ;; not tuple type
                                        (member (car (get-field var-list x)) removed))))
                            (get-field stmts body)))
	]

       ))))
			 
		    
