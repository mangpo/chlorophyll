#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define placeset-collector%
  (class* object% (visitor<%>)
    (super-new)
    ;; save - save placeset into body-placeset field or not.      
    (init-field save actors actors* [need-cf (set)])
    
    (define functions (make-hash))
    (define env (make-hash))
    (define return-map (make-hash))

    (define (make-set x)
      ;(pretty-display `(make-set ,x))
      (cond
       [(rosette-number? x) (set x)]
       [(equal? x #f) (set)]
       [(and (is-a? x Place%) (equal? (get-field at x) "any")) (set)]
       [(is-a? x TypeExpansion%)
	(define ret (set))
	(for ([p (get-field place-list x)])
	     (set! ret (set-union ret (make-set p))))
	ret]
       [(and (list? x) (is-a? (car x) ProxyReturn%)) (set)]
       [(and (list? x) (is-a? (car x) RangePlace%))
	(define ret (set))
	(for ([p x]) (set! ret (set-add ret (get-field place p))))
	ret
	]
       [(is-a? x Place%)
        (let ([at (get-field at x)])
          (if (or (equal? at "any") (equal? at "io"))
              (set)
              (raise "make-set doesn't support Place% that is not @any or @io")))]
       [else (raise `(make-set ,x))]))

    (define/public (set-functions funcs)
      (set! functions (make-hash))
      (for ([f funcs])
	   (hash-set! functions (get-field name f) f)))

    (define/public (visit ast)
      (define (save? s)
        (when save (set-field! body-placeset ast s))
        s)
      
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

       [(is-a? ast ModuleCreate%) (set)]
       
       [(is-a? ast FuncCall%)
        (define ret (make-set (get-field place-type ast)))
        (define name (get-field name ast))

        (define (union-in x)
          (when (hash-has-key? actors* name)
                (set! x (set-intersect need-cf x)))
          (set! ret (set-union ret x)))
        
	(cond
         ;; for visitor-unroll (save = #f)
         [(hash-has-key? functions name)
          (union-in (send (hash-ref functions name) accept this))]
         ;; for standlone (save = #t)
         [(hash-has-key? env name)
          (union-in (hash-ref env name))])

        (for ([arg (get-field args ast)])
             (set! ret (set-union ret (make-set (get-field place-type arg)))))

        ;;(pretty-display (format "PLACESET: FuncCall ~a" name))
        ;;(pretty-display `(placeset-before ,ret))
        (when (hash-has-key? actors name)
              (define l (hash-ref actors name))
              ;;(pretty-display `(remove ,l))
              (for ([pair l])
                   (set! ret (set-remove ret (car pair)))))
        ;;(pretty-display `(placeset-after ,ret))
        ret]

       [(is-a? ast VarDecl%)
        (make-set (get-field place ast))]

       [(is-a? ast ArrayDecl%)
        (make-set (get-field place-list ast))]

       [(is-a? ast For%)
        (save? (send (get-field body ast) accept this))]

       [(is-a? ast If%)
        (define cond-ret (send (get-field condition ast) accept this))
        (define true-ret (send (get-field true-block ast) accept this))
        (define false-ret
          (if (get-field false-block ast)
              (send (get-field false-block ast) accept this)
              (set)))

        (save? (set-union true-ret false-ret))
        (set-union cond-ret true-ret false-ret)
        ]

       [(is-a? ast While%)
        (define pre-ret (send (get-field pre ast) accept this))
        (define cond-ret (send (get-field condition ast) accept this))
        (define body-ret (send (get-field body ast) accept this))

        (save? body-ret)
        (set-union cond-ret pre-ret body-ret)
        ]

       [(is-a? ast Assign%) 
        (set-union (send (get-field lhs ast) accept this)
                   (send (get-field rhs ast) accept this))]

       [(is-a? ast Return%)
        (set)]

       [(is-a? ast Actor%)
        (define info (get-field info ast))
        (if (second info)
            (set (second info) (third info))
            (set))]

       [(and (is-a? ast Program%) (not save))
        ;;(pretty-display "PLACESET: Program (no save)")
        (define ret (set))
        (for ([stmt (get-field stmts ast)])
             (set! ret (set-union ret (send stmt accept this))))

        ret
        ]

       [(is-a? ast Program%)
        ;;(pretty-display "PLACESET: Program (save)")
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))

        (define main
          (findf (lambda (x) (and (is-a? x FuncDecl%)
                                  (equal? (get-field name x) "main")))
                 (get-field stmts ast)))
        (set-field! body-placeset ast (get-field body-placeset main))
        (get-field body-placeset main)
        ]

       [(is-a? ast Block%)
        (save? (foldl (lambda (stmt all) (set-union all (send stmt accept this)))
                      (set) (get-field stmts ast)))]

       [(is-a? ast FuncDecl%)
        ;;(pretty-display (format "PLACESET: FuncDecl ~a" (get-field name ast)))
        (hash-set! return-map (get-field name ast) (get-field return ast))
        (define ret (save? (set-union (send (get-field args ast) accept this)
                                      (send (get-field body ast) accept this))))
        (when save
              (hash-set! env (get-field name ast) ret))
        ret
        ]

       [else (raise (format "visitor-placeset: unimplemented for ~a" ast))]
       ))

    (define/public (get-actors*-need-cf ast all-main-actors)
      (define actors*-nodes (set))
      (for ([name (hash-keys actors*)])
           (set! actors*-nodes (set-union actors*-nodes (hash-ref env name))))
      
      (set-subtract
       (set-intersect (get-field body-placeset ast)
                      (set-subtract actors*-nodes need-cf))
       ;; subtract main actor nodes.
       all-main-actors))

    (define/public (get-actors*-no-cf-map need-cf)
      (define ret (make-hash))
      (for ([name (hash-keys actors*)])
           (hash-set! ret name
                      (set-subtract (hash-ref env name)
                                    need-cf
                                    ;; CAUTION: subtract return is a hack!
                                    (get-return-set name))))
      ret)

    (define (get-return-set name)
      (define return (hash-ref return-map name))
      (if return
          (make-set (get-field place return))
          (set)))

    ))
        
