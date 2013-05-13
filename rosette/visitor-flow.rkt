#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(struct edge (x y w))

(define flow-generator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field env)

    (define (cross-product x y)
      (define pair-set (flow x y))
      (define x-set (car pair-set))
      (define y-set (cdr pair-set))
      (define edges (list))
      (for* ([x x-set]
             [y y-set])
            (when (not (equal? x y))
                  (set! edges (cons (edge x y 1) edges))))
      edges)
        
    (define (multiply edges c)
      (for/list ([e edges])
                (edge (edge-x e) (edge-y e) (* (edge-w e) c))))

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
        (cross-product (get-field index ast) ast)]
       
       [(is-a? ast UnaExp%)
        (let ([e1 (get-field e1 ast)])
          (append (send e1 accept this)
                  (cross-product ast (get-field e1 ast))))]
       
       [(is-a? ast BinExp%)
        (let ([e1 (get-field e1 ast)]
              [e2 (get-field e2 ast)])
          (append (send e1 accept this)
                  (send e2 accept this)
                  (cross-product (get-field e1 ast) ast)
                  (cross-product (get-field e2 ast) ast)))]

       [(is-a? ast FuncCall%)
        (let ([edges (list)]
              [func-ast (car (lookup env ast))])
          (for ([param (get-field stmts (get-field args func-ast))] ; signature
                [arg   (get-field args ast)]) ; actual
               (set! edges (append (cross-product param arg) edges))
               (set! edges (append (send arg accept this) edges)))
          edges)]

       [(is-a? ast For%)
        (multiply (send (get-field body ast) accept this) 
                  (- (get-field to ast) (get-field from ast)))
        ]

       [(is-a? ast If%)
        (let ([condition (get-field condition ast)]
              [true-block (get-field true-block ast)]
              [false-block (get-field false-block ast)])
          (define ret
            (append (send condition accept this)
                    (multiply (send true-block accept this) 2)
                    (cross-product condition (get-field firstexp true-block))))
          (if false-block
              (append (multiply (send false-block accept this) 2)
                      (cross-product condition (get-field firstexp false-block))
                      ret)
              ret))]

       [(is-a? ast While%)
        (let ([condition (get-field condition ast)]
              [body (get-field body ast)]
              [bound (get-field bound ast)])
          (append
           (multiply
            (append (send condition accept this)
                    (cross-product condition (get-field firstexp body)))
            bound)
           (multiply
            (send body accept this)
            (* 2 bound))))]

       [(is-a? ast Assign%)
        (let ([lhs (get-field lhs ast)]
              [rhs (get-field rhs ast)])
          (append (cross-product lhs rhs)
                  (send lhs accept this)
                  (send rhs accept this)))
        ]
       
       [(is-a? ast Block%)
        (foldl (lambda (stmt edges) (append (send stmt accept this) edges))
               (list) (get-field stmts ast))
        ]

       [(is-a? ast FuncDecl%)
        (send (get-field body ast) accept this)]

       [(is-a? ast Program%)
        (foldl (lambda (decl edges) (append (send decl accept this) edges))
               (list) (get-field decls ast))]

       [(or (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Livable%)
            (is-a? ast ArrayDecl%))
        (list)]
       
       [else (raise (format "Error: flow-generator unimplemented for ~a" ast))]))

    ))
        
            
          
        

       