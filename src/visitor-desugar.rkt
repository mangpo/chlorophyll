#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define desugar%
  (class* object% (visitor<%>)
    (super-new)

    (define (decor-list l dec)
      (map (lambda (x) (ext-name x dec)) l))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast TempDecl%)
         (define type (get-field type ast))
         (define native-type
           (if (string? type)
               type
               (car type)))
         
         (define known (get-field known ast))
         (define entry (get-field expect ast))

         (if (and (not (get-field compact ast)) (> entry 1))
             (for/list ([i (in-range entry)]
                        [p (get-field place-list (get-field place ast))])
                       (new TempDecl%
                            [type native-type]
                            [var-list (decor-list (get-field var-list ast) i)]
                            [known known]
                            [place p]
                            [pos (get-field pos ast)]))
             ast)]
         
        [(is-a? ast VarDecl%)
         ;(pretty-display (format "DESUGAR: VarDecl ~a" (get-field var-list ast)))
         (define type (get-field type ast))
         (define native-type
           (if (string? type)
               type
               (car type)))
         
         (define known (get-field known ast))
         (define entry (get-field expect ast))
         
	 (if (> entry 1)
	     ;; int a::0: int a::1;
             (for/list ([i (in-range entry)]
                        [p (get-field place-list (get-field place ast))])
                        (new (if (is-a? ast Param%) Param%  VarDecl%)
                             [type native-type]
                             [var-list (decor-list (get-field var-list ast) i)]
                             [known known]
                             [place (if p p (get-sym))]
                             [pos (get-field pos ast)]))
	     ;; normal type
	     ast)
         ]
         
         ;; cons scenario only happens at return or temp variables
         ;; (if (string? type)
         ;;     var-decls
         ;;     ;; int::2 a; int a::0; int a::1;
         ;;     (begin
         ;;       (set-field! place ast
         ;;                   (new TypeExpansion% 
         ;;                        [place-list (map (lambda (x) (get-field place x)) var-decls)]))
         ;;       (cons ast var-decls)))
         ;; ]
        
        [(is-a? ast ArrayDecl%)
         ;(pretty-display (format "DESUGAR: VarDecl ~a" (get-field var ast)))
         (define type (get-field type ast)) 
         (define known (get-field known ast))
         (define bound (get-field bound ast))
         (define cluster (get-field cluster ast))
         (define entry (get-field expect ast))

         (define (list->tuple inits)
           (if inits
               (let ([v (make-vector entry (list))])
                 (for ([tuple (reverse inits)]) ; reverse order
                      (unless (= entry (length tuple))
                              (send ast init-mismatch))
                      (for ([val tuple]
                            [i (in-range entry)])
                           ;; cons reverses it back
                           (vector-set! v i (cons val (vector-ref v i))))) 
                 (vector->list v))
               (for/list ([i (in-range entry)]) #f)))
         
	 (if (> entry 1)
	     ;; expanded type
	     (for/list ([i (in-range entry)]
			[p (get-field place-list (get-field place-list ast))]
                        [init (list->tuple (get-field init ast))])
		       (let ([new-name (ext-name (get-field var ast) i)])
			 (new ArrayDecl% [type type]
			      [var new-name]
			      [known known]
			      [place-list p]
                              [init init]
			      [bound bound]
			      [cluster cluster]
			      [pos (get-field pos ast)])))
	     ;; normal type
	     ast)
	 ]
        
        [(is-a? ast Num%)
         ;(pretty-display (format "DESUGAR: Num ~a" (send ast to-string)))
         (define entry (get-field expect ast))
         (if (= entry 1)
             ast
	     (for/list ([i (in-range entry)])
		       (send ast clone)))
             ;; (let ([x (get-field n (get-field n ast))]
             ;;       [max-num (arithmetic-shift 1 n-bit)])
             ;;   (for/list ([i (in-range entry)])
             ;;     (let ([n (modulo x max-num)])
             ;;       (set! x (arithmetic-shift x (- 0 n-bit)))
             ;;       ;; num known-type is already default to true
             ;;       (new Num% [n (new Const% [n n])] [pos (get-field pos ast)])))))
	 ]
        
        
        [(is-a? ast Array%)
         ;(pretty-display (format "DESUGAR: Array ~a" (send ast to-string)))
         
         (define index-ret (send (get-field index ast) accept this))
         (define entry (get-field expect ast))
         (define expand (get-field expand ast))
         (define known-type (get-field known-type ast))
         
         (if (= entry 1)
             (let ([sub (get-field sub ast)])
               (when sub
                     (set-field! name ast (ext-name (get-field name ast) sub)))
               ast
               )
             
             ;; list of ASTs
             (if (= expand 1)
		 (for/list ([i (in-range entry)])
			   (send ast clone))
                 ;; (cons
                 ;;  ast
                 ;;  (for/list ([i (in-range (sub1 entry))])
                 ;;    (new Num% [n (new Const% [n 0])] [pos (get-field pos ast)])))
		 (if (= expand entry)
		     (for/list ([i (in-range expand)]
                                [i-index index-ret])
			      (let ([new-name (ext-name (get-field name ast) i)])
				(new Array% [name new-name]
				     [index i-index]
				     ;; set known-type
				     [known-type known-type]
				     [pos (get-field pos ast)])))
		     (raise (format "Expect tuple with ~a entries, ~a entries are given at ~a"
				    entry expand (send ast to-string))))))
		   
                 ;; (append
                 ;;  (for/list ([i (in-range expand)])
                 ;;    (let ([new-name (ext-name (get-field name ast) i)])
                 ;;      (new Array% [name new-name]
                 ;;           [index (send index clone)]
                 ;;           ;; set known-type
                 ;;           [known-type known-type]
		 ;; 	   [pos (get-field pos ast)])))
                 ;;  (for/list ([i (in-range (- entry expand))])
                 ;;    (new Num% [n (new Const% [n 0])] [pos (get-field pos ast)])))))
         ]
        
        [(is-a? ast Var%)
         (define entry (get-field expect ast))
         (define expand (get-field expand ast))
         (define known-type (get-field known-type ast))
         
         ;(pretty-display (format "DESUGAR: Var ~a" (send ast to-string)))

         (if (= entry 1)
             (let ([sub (get-field sub ast)])
               (when sub
                     (set-field! name ast (ext-name (get-field name ast) sub)))
               ast)
             
             ;; list of ASTs
             (if (= expand 1)
		 (for/list ([i (in-range entry)])
		   (send ast clone))
		 (if (= expand entry)
                     (if (is-a? ast Temp%)
                         (for/list ([i (in-range expand)]
                                    [p (get-field place-list (get-field place-type ast))])
                           (let ([new-name (ext-name (get-field name ast) i)])
                             (new Temp% [name new-name]
                                  [known-type known-type]
                                  [pos (get-field pos ast)]
                                  [compact (get-field compact ast)]
                                  [decl (get-field decl ast)]
                                  [place-type p])))
                         (for/list ([i (in-range expand)])
                           (let ([new-name (ext-name (get-field name ast) i)])
                             (new Var% [name new-name]
                                  ;; set known-type
                                  [known-type known-type]
                                  [pos (get-field pos ast)]))))
		     (raise (format 
                             "Expect tuple with ~a entries, ~a entries are given at ~a"
                             entry expand (send ast to-string))))))
         ]
        
        [(is-a? ast Op%)
         ;(pretty-display (format "DESUGAR: Op ~a" (get-field op ast)))
         (define entry (get-field expect ast))
	 (if (= entry 1)
	     ast
	     (for/list ([i (in-range entry)]
			[place (get-field place-list (get-field place ast))])
		       (new Op% [op (get-field op ast)] [place place] [pos (get-field pos ast)])))]
        
        
        [(is-a? ast UnaExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         (define entry (get-field expect ast))
         (define known-type (get-field known-type ast))
         
         ;(pretty-display (format "DESUGAR: UnaExp ~a" (send ast to-string)))
         (if (= entry 1)
             ast
             (for/list ([i-e1 e1-ret]
                        [i-op op-ret])
               (new UnaExp% [op i-op] [e1 i-e1] 
                    [place-type (get-field place i-op)]
                    [known-type known-type] [pos (get-field pos ast)])))
         ]
        
        [(is-a? ast BinExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         (define entry (get-field expect ast))
         (define known-type (get-field known-type ast))
         
         ;(pretty-display (format "DESUGAR: BinExp ~a" (send ast to-string)))
         (if (= entry 1)
             ast
             (for/list ([i-e1 e1-ret]
                        [i-e2 e2-ret]
                        [i-op op-ret])
               (new BinExp% [op i-op] [e1 i-e1] [e2 i-e2] 
                    [place-type (get-field place i-op)]
                    [known-type known-type] [pos (get-field pos ast)])))
         ]
        
        [(is-a? ast FuncCall%)
         ;(pretty-display (format "DESUGAR: FuncCall ~a" (send ast to-string)))
         (set-field! args ast (flatten 
                               (map (lambda (x) (send x accept this))
                                    (get-field args ast))))
         ast]

	[(is-a? ast Recv%)
	 ast]

        [(is-a? ast Send%)
         (define data-ret (send (get-field data ast) accept this))
         (set-field! data ast data-ret)
         ast]
        
        [(is-a? ast Assign%)
         (define lhs (get-field lhs ast))
         (define rhs (get-field rhs ast))
         ;(pretty-display (format "DESUGAR: Assign ~a ~a" lhs rhs))
         
         ;; visit lhs & rhs
         (define lhs-ret (send lhs accept this))
         (define rhs-ret (send rhs accept this))

         (define entry (get-field expect lhs))

         (if (= entry 1)
             ast
	     (for/list ([i-lhs lhs-ret]
			[i-rhs rhs-ret])
                       (new 
                        (if (is-a? ast AssignTemp%) AssignTemp% Assign%)
                        [lhs i-lhs] [rhs i-rhs] [pos (get-field pos ast)])))
         ]

	[(is-a? ast Return%)
	 (define val-ret (send (get-field val ast) accept this))
	 (define pos (get-field pos ast))
         (define entry (get-field expect ast))

	 (if (= entry 1)
             (list
              (new Assign% [lhs (new Var% [name "#return"] [pos pos] [compact #t])] 
                   [rhs val-ret])
              (begin (set-field! val ast (new Var% [name "#return"] [pos pos]))
                     ast))
             (list
              (for/list ([i (in-range entry)]
                         [i-val val-ret])
                        (new Assign% [lhs (new Var% 
                                               [name (ext-name "#return" i)] 
                                               [pos pos] [compact #t])]
                             [rhs i-val]))
              (begin 
                (set-field! val ast 
                            (for/list ([i (in-range entry)])
                                      (new Var% 
                                           [name (ext-name "#return" i)]
                                           [pos pos] [compact #t])))
                     ast)))
         ]
        
        [(is-a? ast If%)
         ;(pretty-display "DESUGAR: If")

	 (send (get-field condition ast) accept this)
	 (send (get-field true-block ast) accept this)
	 (when (get-field false-block ast)
	       (send (get-field false-block ast) accept this))
	 
	 ast
         ]
        
        [(is-a? ast While%)
         (send (get-field pre ast) accept this)
	 (send (get-field condition ast) accept this)
	 (send (get-field body ast) accept this)
	 
	 ast]
        
        [(is-a? ast For%)
         ;(pretty-display "DESUGAR: For")
         (send (get-field body ast) accept this)
	 ast
         ]
        
        [(is-a? ast FuncDecl%)
         ;(pretty-display (format "DESUGAR: FuncDecl ~a (return)" (get-field name ast)))

         ;; (let* ([return (get-field return ast)]
         ;;        [return-ret (send return accept this)])
         ;;   (when (list? return-ret)
         ;;         (set-field! return ast (new Block% [stmts return-ret]))
         ;;         (set-field! return-print ast return)))

         (when (get-field return ast)
               (send (get-field return ast) accept this))
	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
         ast
         ]
        
        [(is-a? ast Block%)
         ;(pretty-display "DESUGAR: Block")
         (set-field! stmts ast (flatten 
                                (map (lambda (x) (send x accept this))
                                     (get-field stmts ast))))
         ast
         ]
        
        [else
         (raise (format "visitor-desugar: unimplemented for ~a" ast))]
        
        ))))
  
(define desugarer (new desugar%))
