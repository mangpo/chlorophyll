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
      (define (get-place-expansion-ast place n)
        (get-place-expansion ast place n))
      
      (cond
        [(is-a? ast VarDecl%)
         (pretty-display (format "DESUGAR: VarDecl ~a" (get-field var-list ast)))
         (define type (get-field type ast))
         (define native-type
           (if (string? type)
               type
               (car type)))
         
         (define known (get-field known ast))
         (define entry (get-field expect ast))
         
         (pretty-display (format "DESUGAR: VarDecl ~a (after declare)" (get-field var-list ast)))
         (pretty-display `(entry ,entry))
         (define var-decls
           (if (> entry 1)
               ;; int a::0: int a::1;
               (let ([place-expand (get-place-expansion-ast (get-field place ast) entry)])
                 (for/list ([i (in-range entry)]
                            [p place-expand])
                   (new VarDecl% [type native-type]
                        [var-list (decor-list (get-field var-list ast) i)]
                        [known known]
                        [place (if p p (get-sym))])))
               ;; normal type
               ast))
         
         (pretty-display (format "DESUGAR: VarDecl ~a (after compute)" (get-field var-list ast)))
         
         ;; cons scenario only happens at return or temp variables
         (if (string? type)
             var-decls
             ;; int::2 a; int a::0; int a::1;
             (begin
               (set-field! place ast
                           (new TypeExpansion% 
                                [place-list (map (lambda (x) (get-field place x)) var-decls)]))
               (cons ast var-decls)))
         ]
        
        [(is-a? ast ArrayDecl%)
         (pretty-display (format "DESUGAR: VarDecl ~a" (get-field var ast)))
         (define type (get-field type ast)) 
         (define known (get-field known ast))
         (define bound (get-field bound ast))
         (define cluster (get-field cluster ast))
         (define entry (get-field expect ast))
         
         (if (> entry 1)
             ;; expanded type
             (let ([place-expand (get-place-expansion-ast (get-field place-list ast) entry)])
               ;; expanded type return
               (for/list ([i (in-range entry)]
                          [p place-expand])
		 (let ([new-name (ext-name (get-field var ast) i)])
                   (new ArrayDecl% [type type]
				       [var new-name]
				       [known known]
				       [place-list p]
				       [bound bound]
				       [cluster cluster]))))
             ;; normal type
             ast)]
        
        [(is-a? ast Num%)
         (pretty-display (format "DESUGAR: Num ~a" (send ast to-string)))
         (define entry (get-field expect ast))
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
         
         (define index (send (get-field index ast) accept this))
         (define entry (get-field expect ast))
         (define expand (get-field expand ast))
         (define known-type (get-field known-type ast))
         
         (pretty-display (format "DESUGAR: Array ~a (before compute)" (send ast to-string)))
         (if (= entry 1)
             (let ([sub (get-field sub ast)])
               (when sub
                     (set-field! name ast (ext-name (get-field name ast) sub)))
               ast
               )
             
             ;; list of ASTs
             (if (= expand 1)
                 (cons
                  ast
                  (for/list ([i (in-range (sub1 entry))])
                    (new Num% [n (new Const% [n 0])])))
                 (append
                  (for/list ([i (in-range expand)])
                    (let ([new-name (ext-name (get-field name ast) i)])
                      (new Array% [name new-name]
                           [index (send index clone)]
                           ;; set known-type
                           [known-type known-type]
			     [pos (get-field pos ast)])))
                  (for/list ([i (in-range (- entry expand))])
                    (new Num% [n (new Const% [n 0])])))))
         ]
        
        [(is-a? ast Var%)
         (define entry (get-field expect ast))
         (define expand (get-field expand ast))
         (define known-type (get-field known-type ast))
         
         (pretty-display (format "DESUGAR: Var ~a" (send ast to-string)))
         ;; no need to worry about place-type at this step
         (if (= entry 1)
             (let ([sub (get-field sub ast)])
               (when sub
                     (set-field! name ast (ext-name (get-field name ast) sub)))
               ast
               )
             
             ;; list of ASTs
             (if (= expand 1)
                 (cons
                  ast
                  (for/list ([i (in-range (sub1 entry))])
                    (new Num% [n (new Const% [n 0])])))
                 ;; allow cast up
                 (append
                  (for/list ([i (in-range expand)])
                    (let ([new-name (ext-name (get-field name ast) i)])
                      (new Var% [name new-name]
			     ;; set known-type
                           [known-type known-type]
                           [pos (get-field pos ast)])))
                  (for/list ([i (in-range (- entry expand))])
                    (new Num% [n (new Const% [n 0])])))))
         ]
        
        [(is-a? ast Op%)
         (define entry (get-field expect ast))
         (if (= entry 1)
             ast
             (for/list ([i (in-range entry)]
                        [place-expand (get-place-expansion-ast (get-field place ast) entry)])
                     (new Op% [op (get-field op ast)] [place place-expand])))]
        
        
        [(is-a? ast UnaExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         (define entry (get-field expect ast))
         (define known-type (get-field known-type ast))
         
         (pretty-display (format "DESUGAR: UnaExp ~a" (send ast to-string)))
         (if (= entry 1)
             ast
             (for/list ([i-e1 e1-ret]
                        [i-op op-ret])
               (new UnaExp% [op i-op] [e1 i-e1] 
                    [known-type known-type])))
         ]
        
        [(is-a? ast BinExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (define op-ret (send (get-field op ast) accept this))
         (define entry (get-field expect ast))
         (define known-type (get-field known-type ast))
         
         (pretty-display (format "DESUGAR: BinExp ~a" (send ast to-string)))
         (define ret
         (if (= entry 1)
             ast
             (for/list ([i-e1 e1-ret]
                        [i-e2 e2-ret]
                        [i-op op-ret])
               (new BinExp% [op i-op] [e1 i-e1] [e2 i-e2] 
                    [known-type known-type]))))
         (pretty-display (format "DESUGAR: BinExp ~a (done)" (send ast to-string)))
         ret
         ]
        
        [(is-a? ast FuncCall%)
         (pretty-display (format "DESUGAR: FuncCall ~a" (send ast to-string)))
         (set-field! args ast (flatten 
                               (map (lambda (x) (send x accept this))
                                    (get-field args ast))))
         ast
         ]
        
        [(is-a? ast Assign%)
         (define lhs (get-field lhs ast))
         (define rhs (get-field rhs ast))
         (pretty-display (format "DESUGAR: Assign ~a ~a" lhs rhs))
         
         (pretty-display "DESUGAR: Assign (before visit)")
         ;; visit lhs & rhs
         (define lhs-ret (send lhs accept this))
         (define rhs-ret (send rhs accept this))

         (define entry (get-field expect lhs))
         
         (pretty-display "DESUGAR: Assign (after visit)")
         (if (= entry 1)
             ast
	     (for/list ([i-lhs lhs-ret]
			[i-rhs rhs-ret])
		       (new Assign% [lhs i-lhs] [rhs i-rhs])))
         ]
        
        [(is-a? ast If%)
         (pretty-display "DESUGAR: If")
         (send (get-field condition ast) accept this)
	 (send (get-field true-block ast) accept this)
	 (when (get-field false-block ast)
	       (send (get-field false-block ast) accept this))
	 ast
         ]
        
        [(is-a? ast While%)
         (pretty-display "DESUGAR: While")
         (send (get-field condition ast) accept this)
         (send (get-field body ast) accept this)
	 ast
         ]
        
        [(is-a? ast For%)
         (pretty-display "DESUGAR: For")
         (send (get-field body ast) accept this)
	 ast
         ]
        
        [(is-a? ast FuncDecl%)
         (pretty-display (format "DESUGAR: FuncDecl ~a (return)" (get-field name ast)))
	 (define return-ret (send (get-field return ast) accept this))
         (pretty-display (format "DESUGAR: FuncDecl ~a (args)" (get-field name ast)))
	 (send (get-field args ast) accept this)
         (pretty-display (format "DESUGAR: FuncDecl ~a (body)" (get-field name ast)))
         (send (get-field body ast) accept this)

         (when (list? return-ret)
               (set-field! return ast (car return-ret))
               (let ([args-block (get-field args ast)])
                 (set-field! stmts args-block (append (cdr return-ret)
                                                      (get-field stmts args-block)))))
         ast
         ]
        
        [(is-a? ast Block%)
         (pretty-display "DESUGAR: Block")
         (set-field! stmts ast (flatten 
                                (map (lambda (x) (send x accept this))
                                     (get-field stmts ast))))
         ]
        
        [else
         (raise (format "visitor-desugar: unimplemented for ~a" ast))]
        
        ))))
  
  
