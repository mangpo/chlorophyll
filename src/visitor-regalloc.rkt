#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-memory.rkt")

(provide (all-defined-out))

(define registor-allocator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [var-map (make-hash)] [decl-map (make-hash)] [items 0])

    (define debug #f)

    (define/public (visit ast)
      (cond
       [(is-a? ast ArrayDecl%) void]
       [(is-a? ast VarDecl%)
        (for ([name (get-field var-list ast)])
             (when (and (need-mem? name) (not (regexp-match #rx"::" name)))
                   (hash-set! decl-map name ast)
                   (hash-set! var-map name 0)))]

       [(is-a? ast Num%)
        (set! items (add1 items))
        (when debug 
              (pretty-display (format "REGALLOC: Num ~a => ~a" (send ast to-string)
                                      items)))
        ]

       [(is-a? ast Var%)
        (define name (get-field name ast))
        
        (when (hash-has-key? var-map name)
              (if (> items 1)
                  (hash-remove! var-map name)
                  (begin
                    (hash-set! var-map name (+ (hash-ref var-map name) 3))
                    (set-field! address ast (if (= items 0) 't 's)))))

        (unless (on-stack? name)
                (set! items (add1 items)))
        
        (when debug 
              (pretty-display (format "REGALLOC: Var ~a => ~a" (send ast to-string)
                                      items)))
        ]
        
       [(is-a? ast UnaExp%)
	(send (get-field e1 ast) accept this)
        (when debug 
              (pretty-display (format "REGALLOC: UnaExp ~a => ~a" (send ast to-string)
                                      items)))
        ]
        
       [(is-a? ast BinExp%)
	(send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (unless (equal? (get-field op (get-field op ast)) "/%")
                (set! items (sub1 items)))
        (when debug 
              (pretty-display (format "REGALLOC: BinExp ~a => ~a" (send ast to-string)
                                      items)))
	]
        
       [(is-a? ast FuncCall%)
        (for ([arg (get-field args ast)])
             (send arg accept this))

        (define place-type (get-field place-type ast))
        (define return-ret 
          (cond
           [(equal? (get-field name ast) "out") 1]
           [(not place-type) 0]
           [(list? place-type) (length (get-field place-type ast))]
           [(number? place-type) 1]))

        (set! items (+ (- items (length (get-field args ast)))
                       return-ret))
        (when debug 
              (pretty-display (format "REGALLOC: FuncCall ~a => ~a" (send ast to-string)
                                      items)))
        ]
        
       [(is-a? ast Assign%)
	(send (get-field rhs ast) accept this)

        (define lhs (get-field lhs ast))
        (cond
         [(is-a? lhs Array%)
          (send (get-field index lhs) accept this)
          (set! items (- items 2))]

         [(or (list? (get-field place-type lhs)) 
              (on-stack? (get-field name lhs)))
          ;; 1) tuple type 2) tem vars => leave everyting on stack
          void]

         [else
          (define name (get-field name lhs))
          (when (hash-has-key? var-map name)
                (if (= items 1)
                    (set-field! address lhs 't)
                    (raise "visitor-regalloc: there should be only one item on stock for assignment"))
                (hash-set! var-map name (+ (hash-ref var-map name) 2)))
          (set! items (sub1 items))])

        (when debug 
              (pretty-display (format "REGALLOC: Assign ~a = ~a  => ~a"
                                      (send lhs to-string)
                                      (send (get-field rhs ast) to-string)
                                      items)))
        ]

       [(is-a? ast Recv%)
        (set! items (add1 items))
        (when debug 
              (pretty-display (format "REGALLOC: Recv => ~a" items)))
        ]

       [(is-a? ast Return%)
        (set! items 0)]

       [(is-a? ast Send%)
	(send (get-field data ast) accept this)
        (set! items (sub1 items))
        (when debug 
              (pretty-display (format "REGALLOC: Send => ~a" items)))
        ]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (set! items 0)
        (when debug 
              (pretty-display (format "\nREGALLOC: If true  => ~a" items)))
	(send (get-field true-block ast) accept this)
        (set! items 0)
	(when (get-field false-block ast)
              (when debug 
                    (pretty-display (format "\nREGALLOC: If false  => ~a" items)))
	      (send (get-field false-block ast) accept this)
              (set! items 0)
              )]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (set! items 0)
        (when debug 
              (pretty-display (format "\nREGALLOC: While condition  => ~a" items)))
	(send (get-field body ast) accept this)
        (set! items 0)
	]

       [(is-a? ast For%)
        (set! items 0)
        (when debug (pretty-display (format "\nREGALLOC: For")))
	(send (get-field body ast) accept this)
        (set! items 0)
	]
       
       [(is-a? ast FuncDecl%)
        (when debug 
              (pretty-display (format "\nREGALLOC: FuncDecl ~a" (get-field name ast))))
        (set! items 0)
        (set! var-map (make-hash))
        (set! decl-map (make-hash))

        (for ([decl (get-field stmts (get-field args ast))])
             (send decl accept this))
        (send (get-field body ast) accept this)

        (define var
          (car (foldl (lambda (x res) (if (> (cdr x) (cdr res)) x res))
                      (cons #f -1) (hash->list var-map))))
        (pretty-display (format "REGALLOC: ~a, CHOOSE: ~a" var-map var))
        (when var
              (set-field! address (hash-ref decl-map var) var))
        ]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]

       [else
        (raise (format "visitor-regalloc: unimplemented for ~a" ast))]
       ))))