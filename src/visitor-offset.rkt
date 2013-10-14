#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define offset-modifier%
  (class* object% (visitor<%>)
    (super-new)
    ;; Collect iter offset to adjust for loop bound (offset).
    ;; This is not for address field.
    (init-field [iter-map (make-hash)] [offset-map (make-hash)])
    (define debug #f)

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" iter-map)
        (set! iter-map new-env))
      )

    (define (pop-scope)
      (set! iter-map (dict-ref iter-map "__up__")))

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        void]

       [(is-a? ast Array%)
        (when debug (pretty-display (format "visitor-offset: Array ~a offset = ~a" (get-field name ast) (hash-ref offset-map (get-field name ast)))))
        (set-field! offset ast (hash-ref offset-map (get-field name ast)))
	(define index (get-field index ast))
	(send index accept this)
        (define offset (get-field offset ast))
        (when (and (not (get-field simple-expr ast)) 
                   (> offset 0))
          ;; if index is not simple and offset > 0
          (set-field! index ast (new BinExp% [op (new Op% [op "-"])]
                                     [e1 index] 
                                     [e2 (new Num% [n (new Const% [n offset])])]))
          (set-field! offset ast 0))

        (for ([x (get-field iter-vars ast)])
             (update iter-map x (cons ast (lookup iter-map x))))
        ]

       [(is-a? ast Var%)
        (pretty-display (format "OFFSET: Var ~a, has-var? ~a"
                                (get-field name ast)
                                (has-var? iter-map (get-field name ast))))]
        
       [(is-a? ast UnaExp%)
	(send (get-field e1 ast) accept this)]

       [(is-a? ast BinExp%)
	(send (get-field e1 ast) accept this)
	(send (get-field e2 ast) accept this)
	]
        
       [(is-a? ast FuncCall%)
	(for ([arg (get-field args ast)])
	     (send arg accept this))
        ]

       [(is-a? ast Recv%)
        void]

       [(is-a? ast Send%)
	(send (get-field data ast) accept this)]
        
       [(is-a? ast Assign%)
	(send (get-field lhs ast) accept this)
	(send (get-field rhs ast) accept this)]

       [(is-a? ast ArrayDecl%)
        (when debug (pretty-display (format "visitor-offset: ArrayDecl ~a offset = ~a" (get-field var ast) (get-field offset ast))))
        (hash-set! offset-map (get-field var ast) (get-field offset ast))]

       [(is-a? ast Return%)
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
	(push-scope)

	(define iter-name (get-field name (get-field iter ast)))
        (define iter-type (get-field iter-type ast))
        (declare iter-map iter-name (list))
	(send (get-field body ast) accept this)

	(define arrays (lookup-name iter-map iter-name))
        (pretty-display (format "OFFSET: For ~a from ~a to ~a" iter-name
                                (get-field from ast) (get-field to ast)))
        (for ([array arrays])
             (display (format "~a, " (send array to-string))))
        (newline)
	(unless (empty? arrays)
		(define min-offset (foldl (lambda (x min-so-far) 
                                            (min (get-field offset x) min-so-far))
					  (get-field to ast) arrays))
                (pretty-display `(min-offset ,min-offset))
		(when (> min-offset 0)
		      (for ([array arrays])
			   (set-field! offset array (- (get-field offset array) min-offset)))
		      (set-field! from ast (- (get-field from ast) min-offset))
		      (set-field! to ast (- (get-field to ast) min-offset))))
	     
	(pop-scope)
	]

       [(is-a? ast FuncDecl%)
	(push-scope)
	(for ([arg (reverse (get-field stmts (get-field args ast)))])
	     (send arg accept this))
	(send (get-field body ast) accept this)
	(pop-scope)
	]
	
       [(is-a? ast Program%)
	(for ([decl (get-field stmts ast)])
	     (send decl accept this))]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]

       [else 
	(raise (format "visitor-offset: unimplemented for ~a" ast))]
       ))))
       