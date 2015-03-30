#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(struct edge (x y w))

(define flow-generator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [functions (make-hash (append '(("in" . ()) ("out" . ()))
					      (for/list ([node digital-nodes])
						(cons (format "digital_write~a"
							      node)
						      (list)))
					      (for/list ([node digital-nodes])
						(cons (format "digital_read~a"
							      node)
						      (list)))
					      (for/list ([node digital-nodes])
						(cons (format "delay_ns~a"
							      node)
						      (list))))
				      )])
    (define debug #f)

    (define (cross-product-raw x y)
      (when debug (pretty-display `(cross-product-raw ,x ,y)))
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
      (when debug (pretty-display `(cross-product ,x-ast ,y-ast)))
      (cross-product-raw (get-field place-type x-ast) (get-field place-type y-ast)))
      
        
    (define (multiply edges c)
      (for/list ([e edges])
                (edge (edge-x e) (edge-y e) (* (edge-w e) c))))

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
        (when debug (pretty-display (format "FLOW: Array ~a" (send ast to-string))))
        (cross-product (get-field index ast) ast)]
       
       [(is-a? ast UnaExp%)
        (when debug (pretty-display (format "FLOW: UnaExp ~a" (send ast to-string))))
        (let ([e1 (get-field e1 ast)])
          (append (send e1 accept this)
                  (cross-product ast (get-field e1 ast))))]
       
       [(is-a? ast BinExp%)
        (when debug (pretty-display (format "FLOW: BinExp ~a" (send ast to-string))))
        (let ([e1 (get-field e1 ast)]
              [e2 (get-field e2 ast)])
          (append (send e1 accept this)
                  (send e2 accept this)
                  (cross-product (get-field e1 ast) ast)
                  (cross-product (get-field e2 ast) ast)))]

       [(is-a? ast FuncCall%)
        (when debug (pretty-display (format "FLOW: FuncCall ~a" (send ast to-string))))
        (let ([edges (if accurate-flow
			 (hash-ref functions (get-field name ast))
			 (list))]
	      [func-ast (get-field signature ast)])
	  
	  (for ([arg (get-field args ast)])
	       (when debug (pretty-display (format ">> ARG ~a: visit ~a" (send ast to-string)
						   (send arg to-string))))
	       (set! edges (append (send arg accept this) edges)))
	  
	  (for ([param (get-field stmts (get-field args func-ast))] ; signature
		[arg   (flatten-arg (get-field args ast))]) ; actual
	       
	       (set! edges (append (cross-product param arg) edges)))
	  edges)
        ]

       [(is-a? ast ProxyReturn%)
        (list)]

       [(is-a? ast For%)
        (when debug (pretty-display (format "FLOW: For")))
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
        (let ([pre (get-field pre ast)]
	      [condition (get-field condition ast)]
              [body (get-field body ast)]
              [bound (get-field bound ast)])
	  (multiply
	   (append (send pre accept this)
		   (send condition accept this)
		   (send body accept this)
		   (cross-product-raw (get-field place-type condition) 
				      (get-field body-placeset ast)))
	   bound))]
       
       [(is-a? ast AssignTemp%)
        (send (get-field rhs ast) accept this)]

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

       [(is-a? ast Program%)
        (define f
          (foldl (lambda (stmt edges) (append (send stmt accept this) edges))
                 (list) (get-field stmts ast)))

        (if accurate-flow
            (hash-ref functions "main")
            f)
        ]
       
       [(is-a? ast Block%)
        (foldl (lambda (stmt edges) (append (send stmt accept this) edges))
               (list) (get-field stmts ast))
        ]

       [(is-a? ast FuncDecl%)
        (when debug (pretty-display (format "\nFLOW: FuncDecl ~a (begin)" (get-field name ast))))
        (define f (send (get-field body ast) accept this))
        (hash-set! functions (get-field name ast) f)
        (when debug (pretty-display (format "FLOW: FuncDecl ~a (end)" (get-field name ast))))
        f
	]

       [(or (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Livable%)
            (is-a? ast ArrayDecl%))
        (list)]
       
       [else (raise (format "Error: flow-generator unimplemented for ~a" ast))]))

    ))
        
            
          
        

       
