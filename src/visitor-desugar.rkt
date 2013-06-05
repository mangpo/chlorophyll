#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define desugar%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [entry 1])

    (define (decore-list l dec)
      (map (lambda (x) (format "~a::~a" x dec)) l))


    (define/public (visit ast)
      (define (get-place-expand place n)
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

      [(is-a? ast VarDecl%)
       (define type (get-field type ast))       
       (decalre env (get-field name ast) (cons type #f))

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
	   ast)]

      [(is-a? ast ArrayDecl%)
       (define type (get-field type ast))    
       (decalre env (get-field name ast) (cons type #f))
       (if (pair? type)
	   ;; expanded type
	   (let* ([n (cdr type)]
                  [place-expand (get-place-expansion (get-field place-list ast) n)])
             ;; expanded type return
	     (for/list ([i (in-range n)]
			[p place])
		       (new ArrayDecl% [type (car type)]
			    [var (format "~a::~a" (get-field var ast) i)]
			    [known (get-field known ast)]
			    [place-list p]
			    [bound (get-field bound ast)]
			    [cluster (get-field cluster ast)])))
	   ;; normal type
	   ast)]

      [(is-a? ast Num%)
       (if (= entry 1)
	   ast
           (let ([x (get-field n (get-field n ast))]
                 [max-num (arithmetic-shift 1 n-bit)])
             (for/list ([i (in-range entry)])
                       (let ([n (modulo x max-num)])
                         (set! x (arithmetic-shift x (- 0 n-bit)))
                         (new Num% [n (new Const% [n n])])))))]

      [(is-a? ast Var%)
       (define type-known (lookup env ast))
       (define type (car type-known))
       (define data-type #f)
       (define entry-type #f)
       (define known-type (cdr type-known))

       (if (string? type)
	   (begin
	     (set! data-type type)
	     (set! entry-type 1))
	   (begin
	     (set! data-type (car type))
	     (set! entry-type (cdr type))))

       ;; no need to worry about place-type at this step
       (if (= entry 1)
           ;; normal AST: modify field
           (begin
             (when (pair? type)
                   (set-field! name (format "~a::~a" (get-field name ast) 0) ast))
             (set-field! known-type ast known-type)
             ast)
	   
           ;; list of ASTs
	   (if (string? type)
               (cons
                ast
                (for/list ([i (in-range (sub1 entry))])
                          (new Num% [n (new Const% [n 0])])))
               (append
                (for/list ([i (in-range to)])
                          (new Var% [name (format "~a::~a" (get-field name ast) i)]
                               [known-type kwown-type]
                               [pos (get-field pos ast)]))
                (for/list ([i (in-range (- entry to))])
                          (new Num% [n (new Const% [n 0])])))))
       ]

      [(is-a? ast UnaExp%)
       (define e1-ret (send (get-field e1 ast) accept this))
       (define op-ret (send (get-field op ast) accept this))

       (if (= entry 1)
           (let ([e1-known (get-field known-type e1)])
             (set-field! known-type ast e-known)
             ast)
           (for/list ([i-e1 e1-ret]
                 [i-op op-ret])
                (new UnaExp% [op i-op] [e1 i-e1])))
       ]

      [(is-a? ast BinExp%)
       (define e1-ret (send (get-field e1 ast) accept this))
       (define e2-ret (send (get-field e2 ast) accept this))
       (define op-ret (send (get-field op ast) accept this))
       
       (if (= entry 1)
           (let ([e1-known (get-field known-type e1)]
                 [e2-known (get-field known-type e2)])
             (set-field! known-type ast (and e1-known e2-known))
             ast)
           (for/list ([i-e1 e1-ret]
                      [i-e2 e2-ret]
                      [i-op op-ret])
                (new BinExp% [op i-op] [e1 i-e1] [e2 i-e2])))
       ]

      [(is-a? ast Assign%)
       (define lhs (get-field lhs ast))
       (define rhs (get-field rhs ast))
       (define type-known (lookup env (get-field name lhs)))
       (define type (car type-known))

       (if (string? type)
           (set! entry 1)
           (set! entry (cdr type)))

       (define lhs-ret (send (get-field lhs ast) accept this))
       (define rhs-ret (send (get-field rhs ast) accept this))
       (define lhs-known (get-field known-type lhs))
       (define rhs-known (get-field known-type rhs))
       (update env lhs (cons type (and lhs-known rhs-known)))

       (if (= entry 1)
           ast
           (for/list ([i-lhs lhs-ret]
                      [i-rhs rhs-ret])
                     (new Assign% [lhs i-lhs] [rhs r-lhs])))
       ]

      [(is-a? ast Block%)
       (for/list ([stmt (get-field stmts ast)])
		 (send stmt accept this))]
       
