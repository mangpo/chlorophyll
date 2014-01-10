#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define placeset-collector%
  (class* object% (visitor<%>)
    (super-new)
    (define functions (make-hash))

    (define (make-set x)
      (cond
       [(rosette-number? x) (set x)]
       [(equal? x #f) (set)]
       [(and (is-a? x Place%) (equal? (get-field at x) "any")) (set)]
       [(is-a? x TypeExpansion%)
	(define ret (set))
	(for ([p (get-field place-list x)])
	     (set! ret (set-union ret (make-set p))))
	ret]
       [(and (list? x) (is-a? (car x) ProxyReturn%)) (set)]
       [else (raise `(make-set ,x))]))

    (define/public (set-functions funcs)
      (set! functions (make-hash))
      (for ([f funcs])
	   (hash-set! functions (get-field name f) f)))

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (make-set (get-field place-type ast))]

       [(is-a? ast Array%)
        (set-union (make-set (get-field place-type ast))
                   (send (get-field index ast) accept this))]

       [(is-a? ast Var%)
        (make-set (get-field place-type ast))]

       [(is-a? ast UnaExp%)
        (set-union (make-set (get-field place-type ast))
                   (send (get-field e1 ast) accept this))]

       [(is-a? ast BinExp%)
        (set-union (make-set (get-field place-type ast))
                   (send (get-field e1 ast) accept this)
                   (send (get-field e2 ast) accept this))]

       [(is-a? ast FuncCall%)
        (define ret (make-set (get-field place-type ast)))
	(when (hash-has-key? functions (get-field name ast))
	      (set! ret (set-union ret 
				   (send (hash-ref functions (get-field name ast)) accept this))))

        (for ([arg (get-field args ast)])
             (set! ret (set-union ret (make-set (get-field place-type arg)))))
        ret]

       [(is-a? ast VarDecl%)
        (make-set (get-field place ast))]

       [(is-a? ast ArrayDecl%)
        (make-set (get-field place ast))]

       [(is-a? ast For%)
        (send (get-field body ast) accept this)]

       [(is-a? ast If%)
        (set-union (send (get-field condition ast) accept this)
                   (send (get-field true-block ast) accept this)
                   (if (get-field false-block ast)
                       (send (get-field false-block ast) accept this)
                       (set)))]

       [(is-a? ast While%)
        (set-union (send (get-field pre ast) accept this)
                   (send (get-field condition ast) accept this)
                   (send (get-field body ast) accept this))]

       [(is-a? ast Assign%) 
        (set-union (send (get-field lhs ast) accept this)
                   (send (get-field rhs ast) accept this))]

       [(is-a? ast Return%)
        (set)]

       [(is-a? ast Block%)
        (foldl (lambda (stmt all) (set-union all (send stmt accept this)))
               (set) (get-field stmts ast))]

       [(is-a? ast FuncDecl%)
        (set-union (send (get-field args ast) accept this)
                   (send (get-field body ast) accept this))]

       [else (raise (format "visitor-placeset: unimplemented for ~a" ast))]
       ))))
        
