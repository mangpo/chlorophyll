#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; performs initial transformations

(define constant-defs #hash(;;for digital io
                            ("HIGH_IMPEDANCE" . 0) ;;Tristate
                            ("WEAK_PULLDOWN" . 1) ;; ~47KOhm
                            ("LOW" . 2) ;;Sink ≤40mA to Vs
                            ("SINK" . 2)
                            ("HIGH" . 3) ;;Source ≤40mA from Vdd
                            ("SOURCE" . 3)))

(define initial%
  (class* object% (visitor<%>)
    (super-new)

    (define/public (visit ast)
      (cond
       [(is-a? ast Var%)
        (let* ([entry (get-field expect ast)]
               [expand (get-field expand ast)]
               [known-type (get-field known-type ast)]
               [name (get-field name ast)])
          (if (hash-has-key? constant-defs name)
              (new Num%
                   [n (new Const%
                           [n (hash-ref constant-defs name #f)]
                           [pos (get-field pos ast)])]
                   [pos (get-field pos ast)])
              ast))]

       [(is-a? ast FuncCall%)
        (set-field! args ast (flatten
                              (map (lambda (x) (send x accept this))
                                   (get-field args ast))))
        ast]

       [(is-a? ast Array%)
        (send (get-field index ast) accept this)
        ast]

       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field op ast) accept this)
        ast]

       [(is-a? ast BinExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (send (get-field op ast) accept this)
        ast]

       [(is-a? ast Send%)
        (let ([data-ret (send (get-field data ast) accept this)])
          (set-field! data ast data-ret))
        ast]

       [(is-a? ast Assign%)
        (set-field! lhs ast (send (get-field lhs ast) accept this))
        (set-field! rhs ast (send (get-field rhs ast) accept this))
        ast]

       [(is-a? ast Return%)
        (send (get-field val ast) accept this)
        ast]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
          (send (get-field false-block ast) accept this))
        ast]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        ast]

       [(is-a? ast For%)
        (send (get-field body ast) accept this)
        ast]

       [(is-a? ast FuncDecl%)
        (when (get-field return ast)
          (send (get-field return ast) accept this))
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)
        ast]

       [(is-a? ast Block%)
        (set-field! stmts ast (flatten
                               (map (lambda (x) (send x accept this))
                                    (get-field stmts ast))))
        ast]

       [else
        ast]))))
