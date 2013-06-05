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
      [(is-a? ast VarDecl%)
       (define type (get-field type ast))       
       (decalre env (get-field name ast) (cons type #f))

       (if (pair? type)
	   ;; expanded type
	   (let [n (cdr type)]
	     (define place (get-field place ast))
	     (if (list? place)
		 (unless (= (length place) n)
			 (send ast partition-mismatch))
		 (set! place (for/list ([i (in-range n)]) 
				       (if place
					   place
					   (get-sym)))))

	     (for/list ([i (in-range n)]
			[p place])
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
	   (let [n (cdr type)]
	     (define place (get-field place-list ast))
	     (if (and (list? place) (not (is-a? (car place) RangePlace%)))
		 ;; place is a list of rangeplace list
		 (unless (= (length place) n)
			 (send ast partition-mismatch))
		 ;; place is a rangeplace list => turn it into a list of rangeplace list
		 (set! place (for/list ([i (in-range n)]) 
				       (map (lambda (x) (send x copy-new-sym)) place)))

	     (for/list ([i (in-range n)]
			[p place])
		       (new ArrayDecl% [type (car type)]
			    [var (format "~a::~a" (get-field var ast) i)]
			    [known (get-field known ast)]
			    [place-list p]
			    [bound (get-field bound ast)]
			    [cluster (get-field cluster ast)]))))
	   ;; normal type
	   ast)]

      [(is-a? ast Num%)
       (if (= entry 1)
	   ast
	   (for/list ([i (in-range entry)])
		     (send ast copy)))]

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

       (set-field! known-type ast known-type)

       ;; no need to worry about place-type at this step
       (if (= entry 1)
	   (cond
	    [(string? type)
	     ast]

	    [else
	     (set-field! name (format "~a::~a" (get-field name ast) 0) ast)])
	   
	   (cond
	    [(string? type)
	     (cons
	      ast
	      (for/list ([i (in-range (sub1 entry))])
			(new Num% [n (new Const% [n 0])])))]

	    [else
	     (define to (max (cdr type) entry))
	     (append
	      (for/list ([i (in-range to)])
			(new Var% [name (format "~a::~a" (get-field name ast) i)]
			     [known-type kwown-type]
			     [pos (get-field pos ast)]))
	      (for/list ([i (in-range (- entry to))])
			(new Num% [n (new Const% [n 0])])))]))
       ]

      [(is-a? ast UnaExp%)
       
       ]

      [(is-a? ast Assign%)
       (send (get-field lhs ast) accept this)
       ]

      [(is-a? ast Block%)
       (for/list ([stmt (get-field stmts ast)])
		 (send stmt accept this))]
       
