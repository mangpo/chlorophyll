#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

(define commcode-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field routing-table part2core)

          
    (define (construct-placelist x-placelist y-placelist index)
      (let* ([x-first (car x-placelist)]
             [y-first (car y-placelist)]
             [x-to (get-field to x-first)]
             [y-to (get-field to y-first)]
             [x-place (get-field place x-first)]
             [y-place (get-field place y-first)])
        (cond 
         [(and (empty? x-placelist) (empty? y-placelist))
          (list)]
         
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
         
         [else (raise "contruct-placelist: unimplemented")])))
    
    (define (gen-path x-ast y-ast)
      (define x (get-field place-type x-ast))
      (define y (get-field place-type y-ast))

      (set-field! send-path x-ast
        (cond
         [(same-place? x y) #f]
         
         [(and (number? x) (number? y))
          (vector-2d-ref routing-table x y)]
         
         [(and (number? x) (place-type-dist? y))
          (for/list ([p (car y)])
                    (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                         [place x] [send-path (vector-2d-ref routing-table x (get-field place p))]))]
         
         [(and (number? y) (place-type-dist? x))
          (for/list ([p (car x)])
                    (let ([place (get-field place p)])
                      (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                           [place place] [send-path (vector-2d-ref routing-table y place)])))]
         
         [(and (place-type-dist? x) 
               (place-type-dist? y) 
               (equal? (send (cdr x) to-string) (send (cdr y) to-string)))
          (let ([x-from (get-field from (caar x))]
                [y-from (get-field to (caar y))])
            (unless (equal? x-from y-from) 
                    (raise "visitor-comminsert: distributions do not start at the same index"))
            (construct-placelist (car x) (car y) x-from))]
         
         [else (raise "gen-path: unimplemented")])))

    ;; TODO turn list into tree!
    (define (get-path-one-to-many from placeset)
      (pretty-display `(get-path-one-to-many ,from ,placeset))
      (for/list ([p placeset])
                (vector-2d-ref routing-table from p)))

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
           [else
            (raise (format "convert-place-type: unimplemented for ~a" p))])))

      (define (convert-placeset)
        (set-field! body-placeset ast
                    (for/list ([p (get-field body-placeset ast)])
                              (vector-ref part2core p))))

      (cond
       [(is-a? ast VarDecl%)
        (unless (equal? (get-field type ast) "void")
                (convert-place))]
        
       [(is-a? ast Livable%)
        (convert-place)]

       [(is-a? ast LivableGroup%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (number? place)
                    (set-field! place-list ast (vector-ref part2core place)))))
	(when (is-a? ast For%) (send (get-field body ast) accept this))]

       [(is-a? ast Num%)
        (convert-place-type)]

       [(is-a? ast Array%)
        (define index (get-field index ast))
        (send index accept this)
        (convert-place-type)
        (gen-path index ast)
        ]

       [(is-a? ast Var%)
        (convert-place-type)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send e2 accept this)
        (send op accept this)
        (convert-place-type)
        (gen-path e1 ast)
        (gen-path e2 ast)
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send op accept this)
	(convert-place-type)
        (gen-path e1 ast)
        ]

       [(is-a? ast FuncCall%)
	(for ([arg (get-field args ast)])
	     (send arg accept this))
        (let ([func-sig (get-field signature ast)])
          (for ([param (get-field stmts (get-field args func-sig))]
                [arg (get-field args ast)])
               (convert-place-type)
               (gen-path arg param)))
        ]

       [(is-a? ast Assign%) 
        (let ([rhs (get-field rhs ast)]
              [lhs (get-field lhs ast)])
          (send rhs accept this)
          (send lhs accept this)
          (gen-path rhs lhs))
        ]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
        (convert-placeset)
        ;; TODO
        (gen-path-condition ast)
        ]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        (convert-placeset)
        ;; TODO
        (gen-path-condition ast)
        ]

       [(is-a? ast Block%) 
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (send (get-field return ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Program%)
        (for ([decl (get-field decls ast)])
             (send decl accept this))]

       [else (raise (format "Error: in partition-to-number, ~a unimplemented!" ast))]
       ))
    ))
            