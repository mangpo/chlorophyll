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

    (define (get-reduce-length map-func [n #f])
      (define map-args (map (lambda (x) (get-field name x)) 
                            (get-field args map-func)))
      (define func (car map-args))
      (define args (cdr map-args))
      (define my-n (hash-ref arrays (car args)))
      (if n
          (unless (= (hash-ref arrays (first args)) n)
            (raise (format "map function cannot be applies to arguments whose number of array entries are not the same as the lhs's. Error at ~a in line ~a." 
                           func (send map-func get-line))))
          (set! n my-n))

      (for ([arg (cdr args)])
           (unless (= (hash-ref arrays arg) n)
             (raise (format "map function cannot be applies to arguments whose number of array entries are not the same as the lhs's. Error at ~a in line ~a." 
                            func (send map-func get-line)))))

      n)

    (define (desugar-map map-func)
      (define map-args (map (lambda (x) (get-field name x)) 
                            (get-field args map-func)))
      (define func (car map-args))
      (define args (cdr map-args))
      (define pos (send map-func get-line))
      (new FuncCallDup% 
           [name func] 
           [args (map (lambda (x) (new Array% 
                                       [name x] [pos pos]
                                       [index (new Var% [name "i"])]))
                      args)]
           [pos pos]))

    (define/public (visit ast)
      (cond
       [(and (is-a? ast Assign%) 
	     (is-a? (get-field rhs ast) FuncCall%) 
	     (equal? (get-field name (get-field rhs ast)) "map"))

        (define lhs-name (get-field name (get-field lhs ast)))
        (define rhs (get-field rhs ast))
        (define n (get-reduce-length rhs (hash-ref arrays lhs-name)))
        (define expanded-func (desugar-map rhs))
        (define pos (get-field pos ast))

	(define body
	  (new Assign% 
	       [lhs (new Array% [pos pos]
                         [name lhs-name] [index (new Var% [name "i"])] [known-type #t])]
	       [rhs expanded-func]
               [pos pos]))
	
        
	(new ParFor% 
             [iter (new Var% [name "i"] [known-type #t])] [from 0] [to n] [pos pos]
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
        (define last-reduce-arg (last reduce-args))
        (define array-map-name (get-field name last-reduce-arg))
	(define n 
          (if (equal? array-map-name "map")
              ;; map reduce case
              (get-reduce-length last-reduce-arg)
              (hash-ref arrays array-map-name)))
        (define pos (get-field pos ast))

        (define array-or-map
          (if (equal? array-map-name "map")
              ;; map reduce case
              (desugar-map last-reduce-arg)
              (new Array% 
                   [name array-map-name] 
                   [index (new Var% [name "i"] [pos pos])]
                   [pos pos])))

        (define reduce-name (get-reduce-name))
	
	(define body
	  (new Assign% 
	       [lhs (new VarDup% [name reduce-name] [pos pos])]
	       [rhs (new FuncCallDup% 
			 [name func] 
			 [args (list (new VarDup% [name reduce-name])
                                     array-or-map)]
                         [pos pos])]
               [pos pos]))
        (define reduce-loop
          (new ParFor% [iter (new Var% [name "i"] [known-type #t] [pos pos])] [from 0] [to n] 
               [body (new Block% [stmts (list body)])]
               [pos pos]))

        (define reduce-decl
          (new VarDeclDup% 
               [var-list (list reduce-name)] 
               [type (hash-ref vartypes lhs-name)] 
               [loop reduce-loop] [pos pos]))
        (define lhs-init
          (new Assign% 
               [lhs (new Var% [name lhs-name] [pos pos])] [rhs (send val clone)] 
               [pos pos]))
        (define reduce-init
          (new BlockDup% 
               [stmts (list (new Assign% 
                                 [lhs (new VarDup% [name reduce-name] [pos pos])] 
                                 [rhs (send val clone)] [pos pos]))]
               [loop reduce-loop] [pos pos]))

        (define reduce-assign
          (new BlockDup% 
               [stmts (list (new Assign% 
                                 [lhs (new Var% [name lhs-name] [pos pos])]
                                 [rhs (new FuncCall% 
                                           [name func] 
                                           [args (list (new Var% [name lhs-name] [pos pos])
                                                       (new VarDup% [name reduce-name] 
                                                            [pos pos]))]
                                           [pos pos])]))]
               [loop reduce-loop]))

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
       
