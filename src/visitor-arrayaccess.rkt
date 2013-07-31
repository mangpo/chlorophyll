#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(provide (all-defined-out))

(define arrayaccess%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [array #f])

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
        (set! array ast)
        (add1 (send (get-field index ast) accept this))]

       [(or (is-a? ast VarDecl%)
            (is-a? ast ArrayDecl%)
            (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Recv%))
        0]
       
       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)]
        
       [(is-a? ast BinExp%)
        (+ (send (get-field e1 ast) accept this)
           (send (get-field e2 ast) accept this))]

       [(is-a? ast Send%)
        (send (get-field data ast) accept this)]

       [(is-a? ast FuncCall%)
        (foldl (lambda (x all) (+ all (send x accept this)))
               0 (get-field args ast))]

       [(is-a? ast Assign%)
        (+ (send (get-field lhs ast) accept this)
           (send (get-field rhs ast) accept this))]

       [(is-a? ast Return%)
        (send (get-field val ast) accept this)]

       [(is-a? ast If%)
        (+ (send (get-field condition ast) accept this)
           (send (get-field true-block ast) accept this)
           (if (get-field false-block ast)
               (send (get-field false-block ast) accept this)
               0))]

       [(is-a? ast While%)
        (+ (send (get-field condition ast) accept this)
           (send (get-field body ast) accept this))]

       [(is-a? ast For%)
        (send (get-field body ast) accept this)]

       [(is-a? ast Block%)
	(foldl (lambda (stmt all) (+ all (send stmt accept this)))
	       0 (get-field stmts ast))]
       
       [else
        (raise (format "visitor-arrayaccess: unimplemented for ~a" ast))]))))
      