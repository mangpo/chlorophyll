#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define (need-mem? name)
  (not (or (regexp-match #rx"_temp" name)
           (regexp-match #rx"_dummy" name)
           (regexp-match #rx"_cond" name)
           (regexp-match #rx"#return" name))))

(define (on-stack? name)
  (or (regexp-match #rx"_dummy" name)
      (regexp-match #rx"_cond" name)
      (regexp-match #rx"#return" name)))

(define memory-mapper%
  (class* object% (visitor<%>)
    (super-new)
    ;; mem-p = data mem pointer
    ;; mem-rp = data mem reduced pointer
    ;; max-temp = number of temp mem neede
    (init-field [mem-map (make-hash)] [mem-p 0] [mem-rp 0] [iter-p 0] 
                [max-temp 0] [max-iter 0])

    (define debug #f)

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" mem-map)
        (set! mem-map new-env))
      )

    (define (pop-scope)
      (set! mem-map (dict-ref mem-map "__up__"))
      )

    (define (gen-mem p rp)
      ;; #t indicates that this is data
      (meminfo p rp #t))

    (define (gen-temp sub)
      ;; treat temp as data
      (if sub
          (meminfo (+ mem-p sub) (+ mem-rp sub) #t)
          (meminfo mem-p mem-rp #t)))
    
    (define (gen-iter p)
      ;; #f indicates that this is temp
      (meminfo p p #f))

    (define (need-temp-mem? name)
      (regexp-match #rx"_temp" name))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast VarDecl%)
        (when debug 
              (pretty-display (format "\nMEMORY: VarDecl ~a" (get-field var-list ast))))
        (define reg-for (get-field address ast))
	(for ([var (get-field var-list ast)])
             (cond 
              [(equal? var reg-for)
               (dict-set! mem-map var 't)
               (set-field! address ast 't)]

              [(need-mem? var)
               (dict-set! mem-map var (gen-mem mem-p mem-rp))
               (set-field! address ast (gen-mem mem-p mem-rp))
               (set! mem-p (add1 mem-p))
               (set! mem-rp (add1 mem-rp))]

              [(need-temp-mem? var)
               (define type (get-field type ast))
               (if (pair? type)
                   (when (> (cdr type) 0)
                         (set! max-temp (cdr type)))
                   (when (= max-temp 0)
                         (set! max-temp 1)))
                       
               ]))
	]

       [(is-a? ast ArrayDecl%)
        (when debug (pretty-display (format "\nMEMORY: ArrayDecl ~a" (get-field var ast))))
	(dict-set! mem-map (get-field var ast) (gen-mem mem-p mem-rp))
        (set-field! address ast (gen-mem mem-p mem-rp))
	(set! mem-p (+ mem-p (get-field bound ast)))
        (set! mem-rp (+ mem-rp (get-field compress ast)))
        ]

       [(is-a? ast Num%)
	void]
        
       [(is-a? ast Array%)
        (when debug 
              (pretty-display (format "\nMEMORY: Array ~a" (send ast to-string))))
	(define index (get-field index ast))
	(define index-ret (send index accept this))
	(set-field! address ast (lookup mem-map ast))
	]
        
       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "\nMEMORY: Var ~a" (send ast to-string))))
	;; (pretty-display `(need-mem? ,(need-mem? (get-field name ast))))
	(cond
         [(need-mem? (get-field name ast))
          (define address (lookup mem-map ast))
          (unless (equal? address 't)
                  (set-field! address ast address))]

         [(need-temp-mem? (get-field name ast))
          (set-field! address ast (gen-temp (get-field sub ast)))
          ]
         )]
        
       [(is-a? ast UnaExp%)
        (when debug 
              (pretty-display (format "\nMEMORY: UnaExp ~a" (send ast to-string))))
	(send (get-field e1 ast) accept this)]
        
       [(is-a? ast BinExp%)
        (when debug 
              (pretty-display (format "\nMEMORY: BinExp ~a" (send ast to-string))))
	(send (get-field e1 ast) accept this)
	(send (get-field e2 ast) accept this)
	]
        
       [(is-a? ast FuncCall%)
        (when debug 
              (pretty-display (format "\nMEMORY: FuncCall ~a" (send ast to-string))))
	(for ([arg (get-field args ast)])
	     (send arg accept this))]

       [(is-a? ast Recv%)
	void]

       [(is-a? ast Send%)
        (when debug 
              (pretty-display (format "\nMEMORY: Send ~a" (get-field port ast))))
	(send (get-field data ast) accept this)]
        
       [(is-a? ast Assign%)
        (when debug 
              (pretty-display (format "\nAssign")))
	(send (get-field lhs ast) accept this)
	(send (get-field rhs ast) accept this)]

       [(is-a? ast Return%)
        (when debug 
              (pretty-display (format "\nReturn")))

        (define val (get-field val ast))
        (if (list? val)
            (for ([v val])
                 (send v accept this))
            (send (get-field val ast) accept this))
	]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
	(push-scope)
	(send (get-field true-block ast) accept this)
	(pop-scope)
	(when (get-field false-block ast)
	      (push-scope)
	      (send (get-field false-block ast) accept this)
	      (pop-scope))]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
	(push-scope)
	(send (get-field body ast) accept this)
	(pop-scope)
	]

       [(is-a? ast For%)
        (when debug (pretty-display (format "MEMORY: For")))
	(push-scope)

	(define iter-name (get-field name (get-field iter ast)))
        (define iter-type (get-field iter-type ast))
        (declare mem-map iter-name (gen-iter iter-p))

        (when (and (number? iter-type) (> iter-type 0))
              (set-field! address ast (gen-iter iter-p)) ; set for itself
              (set! iter-p (add1 iter-p))
              (when (> iter-p max-iter)
                    (set! max-iter iter-p))
              )

	(send (get-field body ast) accept this)

        (when (and (number? iter-type) (> iter-type 0))
              (set! iter-p (sub1 iter-p)))
	     
	(pop-scope)
	]

       [(is-a? ast FuncDecl%)
	;; no memory for return
	(push-scope)
	(for ([arg (reverse (get-field stmts (get-field args ast)))])
	     (send arg accept this))
	(send (get-field body ast) accept this)
	(pop-scope)
	]
	
       [(is-a? ast Program%)
        (when debug (pretty-display (format "MEMORY: Program")))
	(for ([decl (get-field stmts ast)])
	     (send decl accept this))
	(cons (meminfo (+ mem-p max-temp) (+ mem-rp max-temp) null) max-iter)]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]

       [else 
	(raise (format "visitor-memory: unimplemented for ~a" ast))]
       ))))
