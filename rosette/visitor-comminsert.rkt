#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

(define debug #f)

(define commcode-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field routing-table part2core)

          
    (define (construct-placelist x-placelist y-placelist index)
      (if (and (empty? x-placelist) (empty? y-placelist))
          (list)
          (let* ([x-first (car x-placelist)]
                 [y-first (car y-placelist)]
                 [x-to (get-field to x-first)]
                 [y-to (get-field to y-first)]
                 [x-place (get-field place x-first)]
                 [y-place (get-field place y-first)])
            (cond 
             
             [(equal? x-to y-to)
              (cons (new RangePlace% [from index] [to x-to] 
                         [place x-place]
                         [send-path (vector-2d-ref routing-table x-place y-place)])
                    (construct-placelist (cdr x-placelist) (cdr y-placelist) x-to))]
             
             [(< x-to y-to)
              (cons (new RangePlace% [from index] [to x-to]
                         [place x-place]
                         [send-path (vector-2d-ref routing-table x-place y-place)])
                    (construct-placelist (cdr x-placelist) y-placelist x-to))]
             
             [(> x-to y-to)
              (cons (new RangePlace% [from index] [to y-to]
                         [place x-place]
                         [send-path (vector-2d-ref routing-table x-place y-place)])
                    (construct-placelist x-placelist (cdr y-placelist) y-to))]
             
             [else (raise "contruct-placelist: unimplemented")]))))
      
    (define (gen-path x-ast y-ast)
      (define x (get-field place-type x-ast))
      (define y (get-field place-type y-ast))
      (when debug
        (pretty-display `(gen-path ,(send x-ast to-string) ,x  ,(send y-ast to-string) ,y)))

      (set-field! send-path x-ast
        (cond
         [(same-place? x y) #f]
         
         [(and (number? x) (number? y))
          (vector-2d-ref routing-table x y)]
         
         [(and (number? x) (place-type-dist? y))
          (cons
           (for/list ([p (car y)])
                     (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                          [place x] [send-path (vector-2d-ref routing-table x (get-field place p))]))
           (cdr y))]
         
         [(and (number? y) (place-type-dist? x))
          (cons 
           (for/list ([p (car x)])
                     (let ([place (get-field place p)])
                       (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                            [place place] [send-path (vector-2d-ref routing-table place y)])))
           (cdr x))]
         
         [(and (place-type-dist? x) 
               (place-type-dist? y) 
               (equal? (send (cdr x) to-string) (send (cdr y) to-string)))
          (let ([x-from (get-field from (caar x))]
                [y-from (get-field from (caar y))])
            (unless (equal? x-from y-from) 
                    (raise "visitor-comminsert: distributions do not start at the same index"))
            (cons (construct-placelist (car x) (car y) x-from) (cdr x)))]
         
         [else (raise (format "gen-path: unimplemented for ~a and ~a" x y))])))

    ;; TODO turn list into tree!
    (define (get-path-one-to-many from placelist)
      (when debug
        (pretty-display `(get-path-one-to-many ,from ,placelist)))
      (let* ([filtered-list (filter (lambda (x) (not (equal? x from))) placelist)]
             [ret (for/list ([p filtered-list])
                            (vector-2d-ref routing-table from p))])
             (if (empty? ret)
                 #f
                 ret)))

    ;; TODO optimize for if(a[i]) { ... b[i] ... }
    (define (gen-path-condition cond-ast)
      (let ([x (get-field place-type (get-field condition cond-ast))]
            [placeset (get-field body-placeset cond-ast)])
        (set-field! send-path (get-field condition cond-ast)
          (cond
           [(number? x)
            (get-path-one-to-many x placeset)]
         
           [(place-type-dist? x)
            (for/list ([p (car x)])
                      (let ([place (get-field place p)])
                        (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                             [place place] [send-path (get-path-one-to-many place placeset)])))]
           
           [else (raise "gen-path-condition: unimplemented")]))))

    (define/public (visit ast)
      (define (convert-place)
        (let ([p (get-field place ast)])
          (cond
           [(number? p)
            (set-field! place ast (vector-ref part2core p))]
           [(list? p)
            (for ([i p])
                 (send i accept this))]
           [(pair? p)
            (for ([i (car p)])
                 (send i accept this))]
           [else
            (raise (format "convert-place: unimplemented for ~a" p))])))
    
      (define (convert-place-type)
        (let ([p (get-field place-type ast)])
          (cond
           [(number? p)
            (set-field! place-type ast (vector-ref part2core p))]
           [(list? p)
            (for ([i p])
                 (send i accept this))]
           [(pair? p)
            (for ([i (car p)])
                 (send i accept this))]
           [(and (is-a? p Place%) (equal? (get-field at p) "any"))
            void]
           [else
            (raise (format "convert-place-type: unimplemented for ~a" p))])))

      (define (convert)
        (unless (get-field convert ast)
                (set-field! convert ast #t)
                (when (field-bound? place ast)
                      (convert-place))
                (when (field-bound? place-type ast)
                      (convert-place-type))))

      (define (convert-placeset)
        (set-field! body-placeset ast
                    (for/list ([p (get-field body-placeset ast)])
                              (vector-ref part2core p))))

      (cond
       [(is-a? ast Param%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: ~a" (send ast to-string))))
        (send ast pretty-print)
        (convert)
        (send ast pretty-print)
        ]
        
       [(is-a? ast VarDecl%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: VarDecl ~a" (get-field var-list ast))))
        (unless (equal? (get-field type ast) "void")
                (convert))]
        
       [(is-a? ast Livable%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Livable")))
        (when  (is-a? ast RangePlace%)
              (pretty-display (format "COMMINSERT: RangePlace before ~a" (send ast to-string))))
        (convert)

        (when  (is-a? ast RangePlace%)
              (pretty-display (format "COMMINSERT: RangePlace after ~a" (send ast to-string))))

        ]

       [(is-a? ast LivableGroup%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: LivableGroup")))

        (when (is-a? ast ArrayDecl%)
              (pretty-display (format "COMMINSERT: ArrayDecl")))

        (send ast pretty-print)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (number? place)
                    (set-field! place-list ast (vector-ref part2core place)))))

        (send ast pretty-print)
	(when (is-a? ast For%) (send (get-field body ast) accept this))]

       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Num ~a" (send ast to-string))))
        (convert)]

       [(is-a? ast Array%)
        (define index (get-field index ast))
        (send index accept this)
        (convert)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Array ~a" (send ast to-string))))
        (gen-path index ast)
        (define index-sp (get-field send-path index))
        (pretty-display `(Index send-path ,index-sp))
        (when (place-type-dist? index-sp)
              (for ([p (car index-sp)])
                   (pretty-display `(send-path ,(get-field send-path p)))))
        (send ast pretty-print)
        ]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "COMMINSERT: Var ~a" (send ast to-string))))
        (convert)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send e2 accept this)
        (send op accept this)
        (convert)
        (when debug 
              (pretty-display (format "COMMINSERT: BinExp ~a" (send ast to-string))))
        (gen-path e1 ast)
        (gen-path e2 ast)
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send op accept this)
	(convert)
        (when debug 
              (pretty-display (format "COMMINSERT: UnaExp ~a" (send ast to-string))))
        (gen-path e1 ast)
        ]

       [(is-a? ast FuncCall%)
	(for ([arg (get-field args ast)])
	     (send arg accept this))
        (when debug 
              (pretty-display (format "COMMINSERT: FuncCall ~a" (send ast to-string))))
        (let ([func-sig (get-field signature ast)])
          (for ([param (get-field stmts (get-field args func-sig))]
                [arg (get-field args ast)])
               (convert)
               (gen-path arg param)))
        ]

       [(is-a? ast Assign%) 
        (let ([rhs (get-field rhs ast)]
              [lhs (get-field lhs ast)])
          (send rhs accept this)
          (send lhs accept this)
          (when debug 
                (pretty-display (format "COMMINSERT: Assign")))
          (gen-path rhs lhs))
        ]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
        (convert-placeset)
        ;; TODO
        (when debug 
              (pretty-display (format "COMMINSERT: If")))
        (gen-path-condition ast)
        ]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        (convert-placeset)
        ;; TODO
        (when debug 
              (pretty-display (format "COMMINSERT: While")))
        (gen-path-condition ast)
        ]

       [(is-a? ast Block%) 
        (when debug 
              (pretty-display (format "COMMINSERT: Block")))
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (when debug 
              (pretty-display (format "COMMINSERT: FuncDecl ~a" (get-field name ast))))
        (send (get-field return ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Program%)
        (for ([decl (get-field decls ast)])
             (send decl accept this))]

       [else (raise (format "Error: in partition-to-number, ~a unimplemented!" ast))]
       ))
    ))
            