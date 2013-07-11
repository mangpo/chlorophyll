#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define desugar%
  (class* object% (visitor<%>)
    (super-new)

    (define (decor-list l dec)
      (map (lambda (x) (ext-name x dec)) l))

    (define (get-op exp)
      (get-field op (get-field op exp)))

    (define (get-e1 exp)
      (get-field e1 exp))

    (define (get-e2 exp)
      (get-field e2 exp))

    (define (binop-equal? exp str)
      (and (is-a? exp BinExp%) (equal? (get-op exp) str)))
    
    (define (minus e1 e2 place)
      (if (and (is-a? e2 Num%) (= 0 (get-field n (get-field n e2))))
	  e1
	  (new BinExp% [op (new Op% [op "-"] [place place])] [e1 e1] [e2 e2])))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast TempDecl%)
	 ast]

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
		       (new (if (is-a? ast Param%)
				Param%
				VarDecl% )
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
             (let ([x (get-field n (get-field n ast))]
                   [max-num (arithmetic-shift 1 n-bit)])
               (for/list ([i (in-range entry)])
                 (let ([n (modulo x max-num)])
                   (set! x (arithmetic-shift x (- 0 n-bit)))
                   ;; num known-type is already default to true
                   (new Num% [n (new Const% [n n])] [pos (get-field pos ast)])))))]
        
        
        [(is-a? ast Array%)
         ;(pretty-display (format "DESUGAR: Array ~a" (send ast to-string)))
         
         (define index (send (get-field index ast) accept this))
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
                 (cons
                  ast
                  (for/list ([i (in-range (sub1 entry))])
                    (new Num% [n (new Const% [n 0])] [pos (get-field pos ast)])))
                 (append
                  (for/list ([i (in-range expand)])
                    (let ([new-name (ext-name (get-field name ast) i)])
                      (new Array% [name new-name]
                           [index (send index clone)]
                           ;; set known-type
                           [known-type known-type]
			   [pos (get-field pos ast)])))
                  (for/list ([i (in-range (- entry expand))])
                    (new Num% [n (new Const% [n 0])] [pos (get-field pos ast)])))))
         ]
        
        [(is-a? ast Var%)
         (define entry (get-field expect ast))
         (define expand (get-field expand ast))
         (define known-type (get-field known-type ast))
         
         ;(pretty-display (format "DESUGAR: Var ~a" (send ast to-string)))
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
                    (new Num% [n (new Const% [n 0] [pos (get-field pos ast)])])))
                 ;; allow cast up
                 (append
                  (for/list ([i (in-range expand)])
                    (let ([new-name (ext-name (get-field name ast) i)])
                      (new Var% [name new-name]
			     ;; set known-type
                           [known-type known-type]
                           [pos (get-field pos ast)])))
                  (for/list ([i (in-range (- entry expand))])
                    (new Num% [n (new Const% [n 0])] [pos (get-field pos ast)])))))
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
		       (new Assign% [lhs i-lhs] [rhs i-rhs] [pos (get-field pos ast)])))
         ]

	[(is-a? ast Return%)
	 (define val-ret (send (get-field val ast) accept this))
	 (define pos (get-field pos ast))
         (define entry (get-field expect ast))

	 (if (= entry 1)
             (list
              (new Assign% [lhs (new Var% [name "#return"] [pos pos])] [rhs val-ret])
              (begin (set-field! val ast (new Var% [name "#return"] [pos pos]))
                     ast))
             (list
              (for/list ([i (in-range entry)]
                         [i-val val-ret])
                        (new Assign% [lhs (new Var% 
                                               [name (ext-name "#return" i)] 
                                               [pos pos])]
                             [rhs i-val]))
              (begin 
                (set-field! val ast 
                            (for/list ([i (in-range entry)])
                                      (new Var% 
                                           [name (ext-name "#return" i)]
                                           [pos pos])))
                     ast)))
         ]
        
        [(is-a? ast If%)
         ;(pretty-display "DESUGAR: If")

	 (define exp (get-field condition ast))
	 (define t (get-field true-block ast))
	 (define f (get-field false-block ast))
	 (define place (and (is-a? exp BinExp%) (get-field place (get-field op exp))))

	 (define new-if
	   (cond
	    [(binop-equal? exp "!=")
	     (new If!=0% [condition (minus (get-e1 exp) (get-e2 exp))] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp "==")
	     (new If!=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [(binop-equal? exp "<")
	     (new If<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp ">")
	     (new If<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [true-block t]
		  [false-block f])]
	    
	    [(binop-equal? exp ">=")
	     (new If<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [(binop-equal? exp "<=")
	     (new If<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [true-block (if f f (new Block% [stmts (list)]))]
		  [false-block t])]
	    
	    [else
	     ast]))
	   
         (send (get-field condition new-if) accept this)
	 (send (get-field true-block new-if) accept this)
	 (when (get-field false-block new-if)
	       (send (get-field false-block new-if) accept this))
	 
	 new-if
         ]
        
        [(is-a? ast While%)

	 (define exp (get-field condition ast))
	 (define t (get-field body ast))
	 (define place (and (is-a? exp BinExp%) (get-field place (get-field op exp))))

	 (define new-while
	   (cond
	    [(binop-equal? exp "!=")
	     (new While!=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "==")
	     (new While==0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "<")
	     (new While<0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp ">")
	     (new While<0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp ">=")
	     (new While>=0% [condition (minus (get-e1 exp) (get-e2 exp) place)] 
		  [body t])]
	    
	    [(binop-equal? exp "<=")
	     (new While>=0% [condition (minus (get-e2 exp) (get-e1 exp) place)] 
		  [body t])]
	    
	    [else
	     ast]))
	 
         ;(pretty-display "DESUGAR: While")
         (send (get-field condition new-while) accept this)
         (send (get-field body new-while) accept this)
	 new-while
         ]
        
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

	 (send (get-field return ast) accept this)
	 (send (get-field args ast) accept this)
         (send (get-field body ast) accept this)
         ast
         ]
        
        [(is-a? ast Block%)
         ;(pretty-display "DESUGAR: Block")
         (set-field! stmts ast (flatten 
                                (map (lambda (x) (send x accept this))
                                     (get-field stmts ast))))
         ]
        
        [else
         (raise (format "visitor-desugar: unimplemented for ~a" ast))]
        
        ))))
  
(define desugarer (new desugar%))
