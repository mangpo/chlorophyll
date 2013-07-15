#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define memory-mapper%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [mem-map (make-hash)] [mem-p 0] [mem-rp 0] [iter-p 0] [max-iter 0]
		;; collect iter offset to adjust for loop bound
		[iter-map (make-hash)])

    (define debug #f)

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" mem-map)
        (set! mem-map new-env))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" iter-map)
        (set! iter-map new-env))
      )

    (define (pop-scope)
      (set! mem-map (dict-ref mem-map "__up__"))
      (set! iter-map (dict-ref iter-map "__up__"))
      )

    (define (gen-mem p rp)
      (meminfo p rp #t))
    
    (define (gen-iter p)
      (when (>= p max-iter)
	  (set! max-iter (add1 p)))
      (meminfo p p #f))

    (define (need-mem? name)
      (not (or (regexp-match #rx"_temp" name)
	       (regexp-match #rx"_tmp" name)
	       (regexp-match #rx"#return" name))))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast VarDecl%)
        (when debug 
              (pretty-display (format "\nMEMORY: VarDecl ~a" (get-field var-list ast))))
	(for ([var (get-field var-list ast)])
	     (when (need-mem? var)
		   (dict-set! mem-map var (gen-mem mem-p mem-rp))
                   (set-field! address ast (gen-mem mem-p mem-rp))
		   (set! mem-p (add1 mem-p))
                   (set! mem-rp (add1 mem-rp))))
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
        
        ;; (pretty-display `(INDEX ,index ,(is-a? index Var%) 
        ;;                         ,(has-var? iter-map (get-field name ast))))
	(when (and (is-a? index Var%) (has-var? iter-map (get-field name index)))
              ;; (pretty-display "add to index-map!!!")
	      (update iter-map index (cons ast (lookup iter-map index))))
	]
        
       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "\nMEMORY: Var ~a" (send ast to-string))))
	;; (pretty-display `(need-mem? ,(need-mem? (get-field name ast))))
	(when (need-mem? (get-field name ast))
	      (set-field! address ast (lookup mem-map ast)))]
        
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
        (send (get-field condition ast) accept this)
	(push-scope)
	(send (get-field body ast) accept this)
	(pop-scope)
	]

       [(is-a? ast For%)
        (when debug (pretty-display (format "MEMORY: For")))
	(push-scope)

	(define iter-name (get-field name (get-field iter ast)))
	(declare mem-map iter-name (gen-iter iter-p))
	(declare iter-map iter-name (list))

	(set-field! address ast (gen-iter iter-p)) ; set for itself
	(set! iter-p (add1 iter-p))
	(send (get-field body ast) accept this)
	(set! iter-p (sub1 iter-p))

	(define arrays (lookup-name iter-map iter-name))
        ;; (pretty-display `(ITER ,iter-name ,arrays))
	(unless (empty? arrays)
		(define min-offset (foldl (lambda (x min-so-far) 
                                            (min (get-field offset x) min-so-far))
					  (get-field to ast) arrays))
                ;; (pretty-display `(min-offset ,min-offset))
		(when (> min-offset 0)
		      (for ([array arrays])
			   (set-field! offset array (- (get-field offset array) min-offset)))
		      (set-field! from ast (- (get-field from ast) min-offset))
		      (set-field! to ast (- (get-field to ast) min-offset))))
	     
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
	(cons (meminfo mem-p mem-rp null) max-iter)]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]

       [else 
	(raise (format "visitor-memory: unimplemented for ~a" ast))]
       ))))
