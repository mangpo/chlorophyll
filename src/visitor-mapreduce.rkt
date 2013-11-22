#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define mapper-reducer%
  (class* object% (visitor<%>)
    (super-new)
    (define functions (make-hash))
    (define arrays (make-hash))
    (define vartypes (make-hash))
    (define count 0)

    (define (get-reduce-name)
      (set! count (add1 count))
      (format "_red~a" count))

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

       [(and (is-a? ast Assign%) 
	     (is-a? (get-field rhs ast) FuncCall%) 
	     (equal? (get-field name (get-field rhs ast)) "reduce"))

        (define lhs-name (get-field name (get-field lhs ast)))
        (define reduce-args (get-field args (get-field rhs ast)))
        (define func (get-field name (first reduce-args)))
        (unless (= (length reduce-args) 3)
                (raise (format "reduce function only takes 3 arguments. Error at ~a in line ~a" func (send ast get-line))))

        (define val (second reduce-args))
        (define array-name (get-field name (last reduce-args)))
	(define n (hash-ref arrays array-name))
        (define reduce-name (get-reduce-name))

	
        (define reduce-decl
          (new VarDeclDup% 
               [var-list (list reduce-name)] 
               [type (hash-ref vartypes lhs-name)]))
        (define lhs-init
          (new Assign% [lhs (new Var% [name lhs-name])] [rhs (send val clone)]))
        (define reduce-init
          (new AssignDup% [lhs (new VarDup% [name reduce-name])] [rhs (send val clone)]))

	(define body
	  (new Assign% 
	       [lhs (new VarDup% [name reduce-name])]
	       [rhs (new FuncCallDup% 
			 [name func] 
			 [args (list (new VarDup% [name reduce-name])
                                     (new Array% [name array-name] 
                                          [index (new Var% [name "i"])]))])]))

        (define reduce-assign
          (new AssignDup% 
	       [lhs (new Var% [name lhs-name])]
	       [rhs (new FuncCall% 
			 [name func] 
			 [args (list (new Var% [name lhs-name])
                                     (new VarDup% [name reduce-name]))])]))
        (define reduce-loop
          (new For% [iter (new Var% [name "i"] [known-type #t])] [from 0] [to n] 
               [body (new Block% [stmts (list body)])]
               [reduce (list reduce-decl reduce-init reduce-assign)]))

        (list reduce-decl lhs-init reduce-init reduce-loop reduce-assign)
	]

       [(is-a? ast ArrayDecl%)
	(hash-set! arrays (get-field var ast) (get-field bound ast))
	ast]

       [(is-a? ast VarDecl%)
        (for ([v (get-field var-list ast)])
             (hash-set! vartypes v (get-field type ast)))
        ast]

       [(is-a? ast FuncDecl%)
	(send (get-field body ast) accept this)
	(hash-set! functions (get-field name ast) ast)
	ast]

       [(is-a? ast Block%)
	(set-field! stmts ast 
                    (flatten (map (lambda (x) (send x accept this)) 
                                  (get-field stmts ast))))
	ast]
      
       [else
	ast]))))
       
