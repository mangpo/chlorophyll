#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(struct edge (x y w))

(define flow-generator%
  (class* object% (visitor<%>)
    (super-new)

    (define (cross-product-raw x y)
      ;; (pretty-display `(cross-product-raw ,x ,y))
      (define pair-set (flow x y))
      (define x-set (car pair-set))
      (define y-set (cdr pair-set))
      (define edges (list))
      (for* ([x x-set]
             [y y-set])
            (unless (same-place? x y)
                    (set! edges (cons (edge x y 1) edges))))
      edges)
    
    (define (cross-product x-ast y-ast)
      ;; (pretty-display `(cross-product ,x-ast ,y-ast))
      (cross-product-raw (get-field place-type x-ast) (get-field place-type y-ast)))
      
        
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
              [func-ast (get-field signature ast)])
          (for ([param (get-field stmts (get-field args func-ast))] ; signature
                [arg   (get-field args ast)]) ; actual
               (set! edges (append (cross-product param arg) edges))
               (set! edges (append (send arg accept this) edges)))

          (when (is-a? func-ast FilterIOFuncDecl%)
            (define name (get-field name ast))
            (define filter (get-field filter func-ast))
            (cond
              [(equal? name "in")
               (set! edges
                 (append (cross-product-raw
                           (get-field place (get-field output (get-field input-src filter)))
                           (get-field place (get-field input filter)))
                         edges))]
              [(equal? name "out")
               (set! edges
                 (append (cross-product-raw
                           (get-field place (get-field output filter))
                           (get-field place (get-field input (get-field output-dst filter))))
                         edges))]
              )
            )
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
                    (send true-block accept this)
                    (cross-product-raw (get-field place-type condition) 
                                       (get-field body-placeset ast))))
          (if false-block
              (append (send false-block accept this)
                      ret)
              ret))]

       [(is-a? ast While%)
        (let ([condition (get-field condition ast)]
              [body (get-field body ast)]
              [bound (get-field bound ast)])
          (append
           (multiply
            (append (send condition accept this)
                    (cross-product-raw (get-field place-type condition) 
                                       (get-field body-placeset ast)))
            bound)
           (multiply
            (send body accept this)
            bound)))]

       [(is-a? ast Forever%)
        (multiply (send (get-field body ast) accept this) (get-field bound ast))]

       [(is-a? ast Assign%)
        (let ([lhs (get-field lhs ast)]
              [rhs (get-field rhs ast)])
          (append (cross-product lhs rhs)
                  (send lhs accept this)
                  (send rhs accept this)))
        ]

       [(is-a? ast Return%)
	(list)
        ]
       
       [(is-a? ast Block%)
        (foldl (lambda (stmt edges) (append (send stmt accept this) edges))
               (list) (get-field stmts ast))
        ]

       [(is-a? ast FuncDecl%)
        (send (get-field body ast) accept this)]
       
       [(is-a? ast ConcreteFilterDecl%)
        (send (get-field body ast) accept this)]

       [(or (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Livable%)
            (is-a? ast ArrayDecl%))
        (list)]
       
       [else (raise (format "Error: flow-generator unimplemented for ~a" ast))]))

    ))
        
            
          
        

       
