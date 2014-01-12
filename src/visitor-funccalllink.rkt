#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" 
         "visitor-interface.rkt")

(provide (all-defined-out))

(define funccall-linker%
  (class* object% (visitor<%>)
    (super-new)
    (define funcdecls (make-hash))

    (hash-set! funcdecls "in" (get-stdin))
    (hash-set! funcdecls "out" (get-stdout))

    (define/public (visit ast)
      (cond
        [(is-a? ast UnaExp%)
         (send (get-field e1 ast) accept this)]

        [(is-a? ast BinExp%)
         (send (get-field e1 ast) accept this)
         (send (get-field e2 ast) accept this)]

        [(is-a? ast FuncCall%)
         (set-field! signature ast (hash-ref funcdecls (get-field name ast)))
         (for ([arg (get-field args ast)])
              (send arg accept this))
         ]

        [(is-a? ast Send%)
	 (send (get-field data ast) accept this)]

        [(is-a? ast Assign%)
         (send (get-field lhs ast) accept this)
         (send (get-field rhs ast) accept this)]

        [(is-a? ast Return%)
         (define val (get-field val ast))
         (if (list? val)
             (for ([v val])
               (send v accept this))
             (send val accept this))]

        [(is-a? ast If%)
         (send (get-field condition ast) accept this)
         (send (get-field true-block ast) accept this)
         (when (get-field false-block ast)
               (send (get-field false-block ast) accept this))]

        [(is-a? ast While%)
         (send (get-field pre ast) accept this)
         (send (get-field condition ast) accept this)
         (send (get-field body ast) accept this)]

        [(is-a? ast For%)
         (send (get-field body ast) accept this)]

        [(is-a? ast Block%)
         (for ([stmt (get-field stmts ast)])
              (send stmt accept this))]

        [(is-a? ast FuncDecl%)
         (hash-set! funcdecls (get-field name ast) ast)
         (send (get-field body ast) accept this)]
        ))))
         
        
(define (clone x)
  (define ret (send x clone))
  (send ret accept (new funccall-linker%))
  ret)
         