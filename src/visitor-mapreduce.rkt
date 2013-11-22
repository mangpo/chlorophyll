#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define mapper-reducer%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [functions (make-hash)] [arrays (make-hash)])

    (define/public (visit ast)
      (cond
       [(and (is-a? ast Assign%) 
	     (is-a? (get-field rhs ast) FuncCall%) 
	     (equal? (get-field name (get-field rhs ast)) "map"))
	
	(define lhs-name (get-field name (get-field lhs ast)))
	(define n (hash-ref arrays lhs-name))
	(define map-args (map (lambda (x) (get-field name x)) (get-field args (get-field rhs ast))))
	(define func (car map-args))
	(define args (cdr map-args))
	
	(for ([arg args])
	     (unless (= (hash-ref arrays arg) n)
		     (raise (format "map function cannot be applies to arguments whose number of array entries are not the same as the lhs's. Error at ~a in line ~a." func (send ast get-line)))))

	(define body
	  (new Assign% 
	       [lhs (new Array% [name lhs-name] [index (new Var% [name "i"])] [known-type #t])]
	       [rhs (new FuncCallDup% 
			 [name func] 
			 [args (map (lambda (x) (new Array% [name x] [index (new Var% [name "i"])]))
				    args)])]))
	
	(new For% [iter (new Var% [name "i"] [known-type #t])] [from 0] [to n] 
	     [body (new Block% [stmts (list body)])])
	]

       [(is-a? ast ArrayDecl%)
	(hash-set! arrays (get-field var ast) (get-field bound ast))
	ast]

       [(is-a? ast FuncDecl%)
	(send (get-field body ast) accept this)
	(hash-set! functions (get-field name ast) ast)
	ast]

       [(is-a? ast Block%)
	(set-field! stmts ast (map (lambda (x) (send x accept this)) (get-field stmts ast)))
	ast]
      
       [else
	ast]))))
       
