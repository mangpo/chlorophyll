#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define placeset-collector%
  (class* object% (visitor<%>)
    (super-new)

    (define (make-set x)
      (cond
       [(rosette-number? x) (set x)]
       [(equal? x #f) (set)]
       [(list? x) (raise `(make-set ,x))]
       [else (raise `(make-set ,x))]))

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
        (for ([arg (get-field args ast)])
             (set! ret (set-union ret (make-set (get-field place-type ast)))))
        ret] ;; doesn't include places of the corresponding FuncDecl

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
        