#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt"
         (rename-in "routing.rkt" [route route-explicit]))

(provide commcode-inserter%)

;; First pass [insert-routes #f]:
;;  1) Set body-placeset.
;;
;; Second pass [insert-routes #t]:
;;  1) Insert communication route to send-path field.
;;  2) Insert communication route to output-send-path field of filters.
;;  3) Set body-placeset.
;;
;; Note: this visitor mutates AST.
(define commcode-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field w h insert-routes)

    (define debug #f)

    (define current-filter #f)
    (define all-filters (set))
    (define used-routes (set))

    (define (construct-placelist x-placelist y-placelist index [connected-filter #f])
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
                         [send-path (route x-place y-place connected-filter)])
                    (construct-placelist (cdr x-placelist) (cdr y-placelist) x-to connected-filter))]
             
             [(< x-to y-to)
              (cons (new RangePlace% [from index] [to x-to]
                         [place x-place]
                         [send-path (route x-place y-place connected-filter)])
                    (construct-placelist (cdr x-placelist) y-placelist x-to connected-filter))]
             
             [(> x-to y-to)
              (cons (new RangePlace% [from index] [to y-to]
                         [place x-place]
                         [send-path (route x-place y-place connected-filter)])
                    (construct-placelist x-placelist (cdr y-placelist) y-to connected-filter))]
             
             [else (raise "contruct-placelist: unimplemented")]))))

    (define (get-obstacles [connected-filter #f])
      (when debug (pretty-display `(get-obstacles (used-routes ,used-routes)
                                      (all-filters ,(for/list ([f all-filters])
                                                     (get-field name f)))
                                      (current-filter ,(get-field name current-filter))
                                      (connected-filter ,(if connected-filter
                                                           (get-field name connected-filter)
                                                           #f)))))
      (set-union
        (for*/set ([route used-routes]
                   [place route])
          place)
        (for*/set ([filter all-filters]
                   #:unless (equal? filter current-filter)
                   #:unless (equal? filter connected-filter)
                   [place (get-field body-placeset filter)])
          place)))

    (define (route a b [connected-filter #f])
      (define ret (route-explicit a b w h (get-obstacles connected-filter)))
      (when debug (pretty-display (list 'route-returns ret)))
      ret
      )

    (define/public (get-gen-path x y [connected-filter #f])
      (when debug (pretty-display `(get-gen-path ,x ,y)))

      (cond
        [(same-place? x y) #f]

        [(and (is-a? x TypeExpansion%) (is-a? y TypeExpansion%)) #f]

        [(and (number? x) (number? y))
         (route x y connected-filter)]

        [(and (number? x) (place-type-dist? y))
         (cons
           (for/list ([p (car y)])
                     (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                          [place x] [send-path (route x (get-field place p) connected-filter)]))
           (cdr y))]

        [(and (number? y) (place-type-dist? x))
         (cons 
           (for/list ([p (car x)])
                     (let ([place (get-field place p)])
                       (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                            [place place] [send-path (route place y connected-filter)])))
           (cdr x))]

        [(and (place-type-dist? x) 
              (place-type-dist? y) 
              (equal? (send (cdr x) to-string) (send (cdr y) to-string)))
         (let ([x-from (get-field from (caar x))]
               [y-from (get-field from (caar y))])
           (unless (equal? x-from y-from) 
             (raise "visitor-comminsert: distributions do not start at the same index"))
           (cons (construct-placelist (car x) (car y) x-from connected-filter) (cdr x)))]

        [else (raise (format "gen-path: unimplemented for ~a and ~a" x y))]))

    (define (gen-path x-ast y-ast)
      (define x (get-field place-type x-ast))
      (define y (get-field place-type y-ast))
      (when debug
        (pretty-display `(gen-path ,(send x-ast to-string) ,x  ,(send y-ast to-string) ,y)))

      (set-field! send-path x-ast (get-gen-path x y)))

    ;; TODO turn list into tree!
    (define (get-path-one-to-many from placelist)
      (when debug
        (pretty-display `(get-path-one-to-many ,from ,placelist)))
      (let* ([filtered-list (filter (lambda (x) (not (equal? x from))) (set->list placelist))]
             [ret (for/list ([p filtered-list])
                            (route from p))])
             (if (empty? ret)
                 #f
                 ret)))

    ;; TODO optimize for if(a[i]) { ... b[i] ... }
    (define (gen-path-condition cond-ast)
      (let ([x (get-field place-type (get-field condition cond-ast))]
            [placeset (get-field body-placeset cond-ast)])
        (set-field! send-path cond-ast
          (cond
           [(number? x)
            (get-path-one-to-many x placeset)]
         
           [(place-type-dist? x)
            (for/list ([p (car x)])
                      (let ([place (get-field place p)])
                        (new RangePlace% [from (get-field from p)] [to (get-field to p)]
                             [place place] [send-path (get-path-one-to-many place placeset)])))]
           
           [else (raise "gen-path-condition: unimplemented")]))
        ;`(gen-path-condition ,(get-field condition cond-ast) x ,x placeset ,placeset ,(get-field send-path cond-ast))
        ))

    (define/public (visit ast)
      ;; (define (convert-placeset)
      ;;   (set-field! body-placeset ast
      ;;               (for/list ([p (get-field body-placeset ast)])
      ;;                         (vector-ref part2core p))))
      (define (all-place-from place)
        (cond
         
         [(number? place)
          (set place)]

         [(place-type-dist? place)
          (list->set (map (lambda (x) (get-field place x)) (car place)))]

         [(list? place)
          (list->set (map (lambda (x) (get-field place x)) place))]

	 [(at-any? place)
	  (set)]

         [(is-a? place TypeExpansion%)
          (set)]

         [else
          (raise (format "visitor-comminsert: all-place-from: unimplemented for ~a" place))]))
      
      (define (all-place)
        ;(pretty-display `(all-place ,(get-field place ast)))
        (all-place-from (get-field place ast)))

      (define (all-place-list)
        ;(pretty-display `(all-place-type ,(get-field place-list ast)))
        (all-place-from (get-field place-list ast)))

      (define (all-place-type)
        ;(pretty-display `(all-place-type ,(get-field place-type ast)))
        (all-place-from (get-field place-type ast)))

      (define (all-path-from path)
        ;(pretty-display `(all-path-from ,path))
        (cond 
          [(equal? path #f)
           (set)]
          
          [(list? path)
           (list->set (flatten path))]
          
          [else
           ;; place-type-dist
           (foldl (lambda (x all) (set-union all (all-path-from (get-field send-path x))))
                  (set) (car path))]))

      (define (all-path my-ast)
        (let ([path (get-field send-path my-ast)])
          (if path
                (all-path-from path)
                (set))))
                    

      (cond
       [(is-a? ast Param%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: ~a" (send ast to-string))))
        (all-place)
        ]
        
       [(is-a? ast VarDecl%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: VarDecl ~a, type:~a" 
				      (get-field var-list ast) (get-field type ast))))
        (if (equal? (get-field type ast) "void")
          (set)
          (all-place))
        ]
        
       [(is-a? ast Livable%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Livable")))
        (all-place)
        ]

       [(is-a? ast LivableGroup%)
        (when debug (pretty-display (format "COMMINSERT: ArrayDecl")))
        (when (list? (get-field place-list ast))
          (for ([p (get-field place-list ast)])
               (send p accept this)))

        (all-place-list)
        ]

       [(is-a? ast For%)
        (when (list? (get-field place-list ast))
          (for ([p (get-field place-list ast)])
               (send p accept this)))

        (let ([body-ret (send (get-field body ast) accept this)])
          ;(convert-placeset)
          (set-field! body-placeset ast body-ret)
          body-ret)
        ]

       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Num ~a" (send ast to-string))))
        (all-place-type)
        ]

       [(is-a? ast Array%)
        (define index (get-field index ast))
        (send index accept this)
        (when debug 
              (pretty-display (format "\nCOMMINSERT: Array ~a" (send ast to-string))))
        (when insert-routes (gen-path index ast))
        ;; (define index-sp (get-field send-path index))
        ;; (pretty-display `(Index send-path ,index-sp))
        ;; (when (place-type-dist? index-sp)
        ;;       (for ([p (car index-sp)])
        ;;            (pretty-display `(send-path ,(get-field send-path p)))))
        (set-union (all-place-type) (all-path index))
        ]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "COMMINSERT: Var ~a" (send ast to-string))))
        (all-place-type)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        ;(define op (get-field op ast))

        (define e1-ret (send e1 accept this))
        (define e2-ret (send e2 accept this))
        ;(define op-ret (send op accept this))

        (when debug 
              (pretty-display (format "COMMINSERT: BinExp ~a" (send ast to-string))))
        (when insert-routes (gen-path e1 ast))
        (when insert-routes (gen-path e2 ast))
        (set-union e1-ret e2-ret (all-place-type) (all-path e1) (all-path e2))
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        ;(define op (get-field op ast))

        (define e1-ret (send e1 accept this))
        ;(define op-ret (send op accept this))

        (when debug
              (pretty-display (format "COMMINSERT: UnaExp ~a" (send ast to-string))))
        (when insert-routes (gen-path e1 ast))
        (set-union e1-ret (all-place-type) (all-path e1))
        ]

       [(is-a? ast FuncCall%)
	(when debug 
	      (pretty-display (format "COMMINSERT: FuncCall ~a" (get-field name ast))))
	;; recurse on signature
        ;(define args-ret (send (get-field signature ast) accept this))
	(define func-sig (get-field signature ast))

	(define args-ret 
	  (if (is-a? func-sig IOFuncDecl%)
	      (send func-sig accept this)
	      (get-field body-placeset func-sig)))

	;; recurse on arguments
	(for ([arg (get-field args ast)])
	     (set! args-ret (set-union args-ret (send arg accept this))))
        (when debug 
              (pretty-display (format "COMMINSERT: FuncCall ~a" (send ast to-string))))
        
        (define path-ret (set))
	;; Body-placeset of IO function is empty. Add funccall's place-type to it.
	;; (cond 
	;;  [(equal? (get-field name func-sig) "in")
	;;   (set-field! body-placeset func-sig (set (get-field place-type ast)))]
	;;  [(equal? (get-field name func-sig) "out")
	;;   (let ([arg (car (get-field stmts (get-field args func-sig)))])
	;;     (set-field! body-placeset func-sig (set (get-field place-type arg))))]
	;;  )
	
	;; Generate path for argument to parameter.
	(for ([param (get-field stmts (get-field args func-sig))]
	      [arg (get-field args ast)])
	     (when insert-routes (gen-path arg param))
	     (set! path-ret (set-union path-ret (all-path arg))))

        (define io-path-ret
          (if (is-a? func-sig FilterOutputFuncDecl%)
            (let ([path (get-field output-send-path func-sig)])
              (if path (list->set (drop-right path 1)) (set)))
            (set)))

        (set-union path-ret args-ret io-path-ret
		   (get-field body-placeset (get-field signature ast)) (all-place-type))
        ]

       [(is-a? ast Assign%) 
        (let ([rhs (get-field rhs ast)]
              [lhs (get-field lhs ast)])
          (when debug 
            (pretty-display (format "COMMINSERT: Assign ~a = ~a"
                                    (send lhs to-string) (send rhs to-string))))
          (define lhs-ret (send lhs accept this))
          (define rhs-ret (send rhs accept this))
	  (unless (get-field nocomm ast)
            (when insert-routes (gen-path rhs lhs)))
          (set-union rhs-ret lhs-ret (all-path rhs))
          )
        ]

       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            (for ([x val])
                 (send x accept this))
            (send val accept this))
	(set)]

       [(is-a? ast If%)
        (define cond-ret (send (get-field condition ast) accept this))
        (define true-ret (send (get-field true-block ast) accept this))
        (define false-ret
          (if (get-field false-block ast)
            (send (get-field false-block ast) accept this)
            (set)))
        ;(convert-placeset)
        (when debug 
              (pretty-display (format "COMMINSERT: If")))
        
        (define body-ret (set-union true-ret false-ret))
        (set-field! body-placeset ast body-ret)
        
        (when insert-routes (gen-path-condition ast))
        (set-union body-ret cond-ret (all-path ast))
        ]

       [(is-a? ast While%)
	(define cond-ret (send (get-field condition ast) accept this))
        (define body-ret (send (get-field body ast) accept this))
        ;(convert-placeset)
        ;; TODO
        (when debug 
              (pretty-display (format "COMMINSERT: While")))

        (define ret (set-union cond-ret body-ret))
        (set-field! body-placeset ast ret)

        (when insert-routes (gen-path-condition ast))
        (set-union ret (all-path ast))
        ]

       [(is-a? ast Forever%)
        (define body-ret (send (get-field body ast) accept this))
        (set-field! body-placeset ast body-ret)
        (set-union body-ret (all-path ast))]


       [(is-a? ast Block%) 
        (when debug 
              (pretty-display (format "COMMINSERT: Block")))
        (when (is-a? ast Program%)
          (when debug 
            (pretty-display (format "COMMINSERT: That Block was a Program")))
          (when insert-routes
            (for ([decl (get-field stmts ast)]
                  #:when (is-a? decl ConcreteFilterDecl%))
              (set! all-filters (set-add all-filters decl))
            )))
        
        (define ret (set))
        (for ([stmt (get-field stmts ast)])
             (set! ret (set-union ret (send stmt accept this))))
        ret]

       [(is-a? ast FuncDecl%)
        (when debug 
	      (pretty-display "\n--------------------------------------------")
              (pretty-display (format "COMMINSERT: FuncDecl ~a" (get-field name ast))))
        (define return-ret (send (get-field return ast) accept this))
        (define args-ret (send (get-field args ast) accept this))
        (define body-ret (send (get-field body ast) accept this))

        ;(convert-placeset)
        (let ([ret (set-union return-ret args-ret body-ret)])
          (set-field! body-placeset ast ret)
          ret)
        ]
       
       [(is-a? ast ConcreteFilterDecl%)
        (when debug 
	      (pretty-display "\n--------------------------------------------")
              (pretty-display (format "COMMINSERT: ConcreteFilterDecl ~a" (get-field name ast))))
        (set! current-filter ast)
        (define input-ret (send (get-field input-vardecl ast) accept this))
        (define output-ret (send (get-field output-vardecl ast) accept this))

        (when insert-routes
          (for ([func (get-field output-funcs ast)]
                #:when (is-a? func FilterOutputFuncDecl%))
            (define this-filter (get-field this-filter func))
            (define destination-filter (get-field destination-filter func))
            (define path (get-gen-path (get-field place (get-field output-vardecl this-filter))
                                       (get-field place (get-field input-vardecl destination-filter))
                                       destination-filter))
            (set-field! output-send-path func path)
            (when path
              (define path-obstacle
                (for/list ([place path]
                           #:unless (set-member? (get-field body-placeset this-filter) place)
                           #:unless (set-member? (get-field body-placeset destination-filter) place))
                  place))
              (set! used-routes (set-add used-routes path-obstacle)))))

        (define args-ret (send (get-field args ast) accept this))
        (define body-ret (send (get-field body ast) accept this))

        (set! current-filter #f)
        ;(convert-placeset)
        (let ([ret (set-union input-ret output-ret args-ret body-ret)])
          (set-field! body-placeset ast ret)
          ret)
        ]

       [else (raise (format "visitor-comminsert: ~a unimplemented" ast))]
       ))
    ))

