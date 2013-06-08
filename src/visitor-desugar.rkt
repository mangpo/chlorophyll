#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define desugar%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [array-map (make-hash)] [entry 1])
    ;; env maps
    ;; 1) var-name  -> (cons type known)
    ;; 2) func-name -> func-ast

    ;; type
    ;; 1) data-type
    ;; 2) (cons data-type entry)

    ;; Declare IO function: in(), out(data)
    (declare env "in" stdin)
    (declare env "out" stdout)

    (define (decor-list l dec)
      (map (lambda (x) (ext-name x dec)) l))

    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" array-map)
        (set! array-map new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__"))
      (set! array-map (dict-ref array-map "__up__")))


    (define/public (visit ast)
      (define (get-place-expansion place n)
        (cond
          [(is-a? place TypeExpansion%)
           (unless (= (length (get-field place-list place)) n)
             (send ast partition-mismatch))
           (get-field place-list place)]
          
          [(symbolic? place)
           ;; list of symbolic vars
           (cons place (for/list ([i (in-range (sub1 n))]) (get-sym)))]
          
          [else
           (raise (format "visitor-desugar: get-place-exapand: unimplemented for ~a" place))]))
      
      (cond
        [(is-a? ast VarDecl%)
         (pretty-display (format "DESUGAR: VarDecl ~a" (get-field var-list ast)))
         (define type (get-field type ast)) 
         (define expand
           (if (string? type)
               #f
               (cdr type)))
         
         (define known (get-field known ast))  
         (for ([name (get-field var-list ast)])
           ;; declare type
           (declare env name (cons type known))
           ;; declare native type
           (when expand
             (for ([i (in-range expand)])
               (declare env (ext-name name i) (cons (car type) known))))
           )
         
         (pretty-display (format "DESUGAR: VarDecl ~a (after declare)" (get-field var-list ast)))
         (define ret
         (if (pair? type)
             ;; expanded type
             (let* ([n (cdr type)]
                    [place-expand (get-place-expansion (get-field place ast) n)])
               (for/list ([i (in-range n)]
                          [p place-expand])
                 (new VarDecl% [type (car type)]
                      [var-list (decor-list (get-field var-list ast) i)]
                      [known (get-field known ast)]
                      [place (if p p (get-sym))])))
             ;; normal type
             ast))
         
         (pretty-display (format "DESUGAR: VarDecl ~a (after compute)" (get-field var-list ast)))
         
         ret
         ]
        
        [(is-a? ast ArrayDecl%)
         (pretty-display (format "DESUGAR: VarDecl ~a" (get-field var ast)))
         (define type (get-field type ast)) 
         (define expand
           (if (string? type)
               #f
               (cdr type)))
         
         (define known (get-field known ast))    
         ;; declare type
         (declare env (get-field var ast) (cons type known))
         (declare array-map (get-field var ast) ast)
         
         (if (pair? type)
             ;; expanded type
             (let* ([n (cdr type)]
                    [place-expand (get-place-expansion (get-field place-list ast) n)])
               ;; expanded type return
               (for/list ([i (in-range n)]
                          [p place-expand])
		 (let* ([new-name (ext-name (get-field var ast) i)]
			[new-decl (new ArrayDecl% [type (car type)]
				       [var new-name]
				       [known (get-field known ast)]
				       [place-list p]
				       [bound (get-field bound ast)]
				       [cluster (get-field cluster ast)])])
		   ;; declare native type
		   (when expand
			 (declare env (ext-name (get-field var ast) i) (cons (car type) known))
			 (declare array-map (ext-name (get-field var ast) i) new-decl))
		   new-decl)))
             ;; normal type
             ast)]
        
        [(is-a? ast Num%)
         (pretty-display (format "DESUGAR: Num ~a" (send ast to-string)))
         (if (= entry 1)
             ast
             (let ([x (get-field n (get-field n ast))]
                   [max-num (arithmetic-shift 1 n-bit)])
               (for/list ([i (in-range entry)])
                 (let ([n (modulo x max-num)])
                   (set! x (arithmetic-shift x (- 0 n-bit)))
                   ;; num known-type is already default to true
                   (new Num% [n (new Const% [n n])])))))]
        
        
        [(is-a? ast Array%)
         (pretty-display (format "DESUGAR: Array ~a" (send ast to-string)))
         
         (define old-entry entry)
         (set! entry 1)
         (define index (send (get-field index ast) accept this))
	 (pretty-display `(index ,index))

         (unless (get-field known-type index)
	   (pretty-display `(lookup ,(lookup array-map ast)))
           (set-field! cluster (lookup array-map ast) #t))
         (set! entry old-entry)
         
         (define type-known (lookup env ast))
         (define type (car type-known))
         (define known-type (cdr type-known))
         
         (pretty-display (format "DESUGAR: Array ~a (before compute)" (send ast to-string)))
         (if (= entry 1)
             (begin
               (when (pair? type)
                  (set-field! name ast (ext-name (get-field name ast) 0)))
               ;; set known-type
               (set-field! known-type ast known-type)
               ast)
             
             ;; list of ASTs
             (if (string? type)
                 (cons
                  ast
                  (for/list ([i (in-range (sub1 entry))])
                    (new Num% [n (new Const% [n 0])])))
                 (let ([to (cdr type)])
		   ;; no cast down
                   (when (> to entry) (send ast partition-mismatch (cdr type) entry))
		   ;; allow cast up
                   (append
                    (for/list ([i (in-range to)])
		      (let ([new-name (ext-name (get-field name ast) i)])
			(new Array% [name new-name]
			     [index (send index clone)]
			     ;; set known-type
			     [known-type (cdr (lookup-name env new-name))]
			     [pos (get-field pos ast)])))
                    (for/list ([i (in-range (- entry to))])
                      (new Num% [n (new Const% [n 0])]))))))
         ]
        
        [(is-a? ast Var%)
         (define type-known (lookup env ast))
         (define type (car type-known))
         (define known-type (cdr type-known))
         
         (pretty-display (format "DESUGAR: Var ~a" (send ast to-string)))
         ;; no need to worry about place-type at this step
         (if (= entry 1)
             ;; normal AST: modify field
             (begin
               (when (pair? type)
                  (set-field! name ast (ext-name (get-field name ast) 0)))
               ;; set known-type
               (set-field! known-type ast known-type)
               ast)
             
             ;; list of ASTs
             (if (string? type)
                 (cons
                  ast
                  (for/list ([i (in-range (sub1 entry))])
                    (new Num% [n (new Const% [n 0])])))
                 (let ([to (cdr type)])
		   ;; no cast down
                   (when (> to entry) (send ast partition-mismatch entry))
		   ;; allow cast up
                   (append
                    (for/list ([i (in-range to)])
		      (let ([new-name (ext-name (get-field name ast) i)])
			(new Var% [name new-name]
			     ;; set known-type
			     [known-type (cdr (lookup-name env new-name))]
			     [pos (get-field pos ast)])))
                    (for/list ([i (in-range (- entry to))])
                      (new Num% [n (new Const% [n 0])]))))))
         ]
        
        [(is-a? ast Op%)
         (if (= entry 1)
             ast
             (for/list ([i (in-range entry)]
                        [place-expand (get-place-expansion (get-field place ast) entry)])
                     (new Op% [op (get-field op ast)] [place place-expand])))]
        
        
        [(is-a? ast UnaExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         
         (pretty-display (format "DESUGAR: UnaExp ~a" (send ast to-string)))
         (if (= entry 1)
             (let ([e1-known (get-field known-type e1-ret)])
               ;; set known-type
               (set-field! known-type ast e1-known)
               ast)
             (for/list ([i-e1 e1-ret]
                        [i-op op-ret])
               (new UnaExp% [op i-op] [e1 i-e1] 
                    ;; set known-type
                    [known-type (get-field known-type i-e1)])))
         ]
        
        [(is-a? ast BinExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         
         (pretty-display (format "DESUGAR: BinExp ~a" (send ast to-string)))
         (define ret
         (if (= entry 1)
             (let ([e1-known (get-field known-type e1-ret)]
                   [e2-known (get-field known-type e2-ret)])
               ;; set known-type
               (set-field! known-type ast (and e1-known e2-known))
               ast)
             (for/list ([i-e1 e1-ret]
                        [i-e2 e2-ret]
                        [i-op op-ret])
               (new BinExp% [op i-op] [e1 i-e1] [e2 i-e2] 
                    ;; set known-type
                    [known-type 
                     (and (get-field known-type i-e1) (get-field known-type i-e2))]))))
         (pretty-display (format "DESUGAR: BinExp ~a (done)" (send ast to-string)))
         ret
         ]
        
        [(is-a? ast FuncCall%)
         (define func-ast (lookup env ast))
         (define type (get-field type (get-field return func-ast)))
         
         (when entry
           (if (string? type)
               (unless (= entry 1)
                 (send ast type-mismatch type entry))
               (unless (= entry (cdr type))
                 (send ast type-mistmatch type entry))))
         
         
         (define args (get-field args ast))
         (define params (get-field stmts (get-field args func-ast)))
         
         (define old-entry entry)
         (set-field! args
		     ast
                     (for/list ([arg args]
                                [param params])
                       (let ([param-type (get-field type param)])
                         ;; set entry
                         (if (string? param-type)
                             (set! entry 1)
                             (set! entry (cdr param-type))))
                       
                       ;; visit arg
                       (let ([arg-ret (send arg accept this)])
                         ;; interpret known-type
                         (send param set-known
                               (and (get-field known-type param) (get-field known-type arg)))
                         arg-ret)))
         ;; reset entry
         (set! entry old-entry)
         
         (pretty-display (format "DESUGAR: FuncCall ~a" (send ast to-string)))
         ast
         ]
        
        [(is-a? ast Assign%)
         (define lhs (get-field lhs ast))
         (define rhs (get-field rhs ast))
         (pretty-display (format "DESUGAR: Assign ~a ~a" lhs rhs))
         (define type-known (lookup env lhs))
         (define type (car type-known))
         
         (if (string? type)
             (set! entry 1)
             (set! entry (cdr type)))
         
         (pretty-display "DESUGAR: Assign (before visit)")
         ;; visit lhs & rhs
         (define lhs-ret (send lhs accept this))
         (define rhs-ret (send rhs accept this))
         (define lhs-known (get-field known-type lhs))
         (define rhs-known (get-field known-type rhs))

	 ;; if rhs is not known, update env.
	 (unless rhs-known
		 (update env lhs (cons type rhs-known)))
         
         (pretty-display "DESUGAR: Assign (after visit)")
         (if (= entry 1)
             ast
	     (for/list ([i-lhs lhs-ret]
			[i-rhs rhs-ret])
		       (new Assign% [lhs i-lhs] [rhs i-rhs])))
         ]
        
        [(is-a? ast If%)
         (pretty-display "DESUGAR: If")
         (set! entry 1)
         (send (get-field condition ast) accept this)

	 (push-scope)
	 (send (get-field true-block ast) accept this)
	 (pop-scope)

	 (when (get-field false-block ast)
	       (push-scope)
	       (send (get-field false-block ast) accept this)
	       (pop-scope))
	 ast
         ]
        
        [(is-a? ast While%)
         (pretty-display "DESUGAR: While")
         (set! entry 1)
         (send (get-field condition ast) accept this)
	 (push-scope)
         (send (get-field body ast) accept this)
	 (pop-scope)
	 ast
         ]
        
        [(is-a? ast For%)
         (pretty-display "DESUGAR: For")
	 (push-scope)
	 (declare env (get-field name (get-field iter ast)) (cons "int" #t))
         (send (get-field body ast) accept this)
	 (pop-scope)
	 ast
         ;; TODO
         ;; int::2[] a[10];
         ;; for i {
         ;;   if(i % 2 == 0) {
         ;;     a[i] = ..
         ;;   } 
         ;; }
         ]
        
        [(is-a? ast Program%)
         (pretty-display "DESUGAR: Program")
         (define stmts (get-field stmts ast))
         
         ;; update env front to back
         (for ([stmt stmts])
	      (if (is-a? stmt FuncDecl%)
		  (declare env (get-field name stmt) stmt)
		  (send stmt accept this)))
         
         ;; desugar back to front
         (for ([stmt (reverse stmts)])
           (when (is-a? stmt FuncDecl%)
		 (send stmt accept this)))
         ]
        
        [(is-a? ast Block%)
         (pretty-display "DESUGAR: Block")
         (set-field! stmts ast (flatten (for/list ([stmt (get-field stmts ast)])
                                          (set! entry #f)
                                          (send stmt accept this))))
         ]
        
        
        [(is-a? ast FuncDecl%)
	 (push-scope)
	 (send (get-field return ast) accept this)
	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
	 (pop-scope)
         ]
        
        [else
         (raise (format "visitor-desugar: unimplemented for ~a" ast))]
        
        
        ))))
  
  
