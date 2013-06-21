#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define memory-mapper%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [mem-map (make-hash)] [mem-p 0] [iter-p 0] [max-iter 0])

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" mem-map)
        (set! mem-map new-env)))

    (define (pop-scope)
      (set! mem-map (dict-ref mem-map "__up__")))

    (define (mem p)
      (cons p #t))
    
    (define (iter p)
      (when (>= p max-iter)
	  (set! max-iter (add1 p)))
      (cons p #f))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast VarDecl%)
	(for ([var (get-field var-list ast)])
	     ;; TODO: if not temp or return
	     (dict-set! mem-map var (mem mem-p))
	     (set! mem-p (add1 mem-p)))]

       [(is-a? ast ArrayDecl%)
	(dict-set! mem-map (get-field var ast) (mem mem-p))
	(set! mem-p (+ mem-p (get-field bound ast)))]

       [(is-a? ast Num%)
	void]
        
       [(is-a? ast Array%)
	(send (get-field index ast) accept this)
	(set-field! address ast (lookup mem-map ast))]
        
       [(is-a? ast Var%)
	;; TODO: if not temp or return
	(set-field! address ast (lookup mem-map ast))]
        
       [(is-a? ast UnaExp%)
	(send (get-field e1 ast) accept this)]
        
       [(is-a? ast BinExp%)
	(send (get-field e1 ast) accept this)
	(send (get-field e2 ast) accept this)
	]
        
       [(is-a? ast FuncCall%)
	(for ([arg (get-field args ast)])
	     (send arg accept this))]

       [(is-a? ast Recv%)
	void]

       [(is-a? ast Send%)
	(send (get-field data ast) accept this)]
        
       [(is-a? ast Assign%)
	(send (get-field lhs ast) accept this)
	(send (get-field rhs ast) accept this)]

       [(is-a? ast Return%)
	;; (send (get-field val ast) accept this)
	void
	]

       [(is-a? ast If%)
	(push-scope)
	(send (get-field true-block ast) accept this)
	(pop-scope)
	(when (get-field false-block ast)
	      (push-scope)
	      (send (get-field false-block ast) accept this)
	      (pop-scope))]

       [(is-a? ast While%)
	(push-scope)
	(send (get-field body ast) accept this)
	(pop-scope)
	]

       [(is-a? ast For%)
	(push-scope)
	(dict-set! mem-map (get-field iter ast) (iter iter-p))
	(set-field! address ast (iter iter-p)) ; set for itself
	(set! iter-p (add1 iter-p))
	(send (get-field body ast) accept this)
	(set! iter-p (sub1 iter-p))
	(pop-scope)
	]

       [(is-a? ast FuncDecl%)
	;; no memory for return
	(push-scope)
	(send (get-field args ast) accept this)
	(send (get-field body ast) accept this)
	(pop-scope)
	]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]
	
       [(is-a? ast Program%)
	(for ([decl (get-field stmts ast)])
	     (send decl accept this))
	(cons mem-p max-iter)]

       [else 
	(raise (format "visitor-memory: unimplemented for ~a" ast))]
       ))))
