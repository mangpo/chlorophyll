#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; Make a duplicate of the given AST but set the place-type of the AST
;; specific to the set range and index.
(define range-cloner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [range #f] [index #f])

    (define/public (set-range x-range x-index)
      (set! range x-range)
      (set! index x-index))

    (define/public (visit ast)
      (define (get-known-type)
        (get-field known-type ast))

      (define (get-place-type)
        (let ([place-type (get-field place-type ast)])
          (if (and (place-type-dist? place-type) 
                   (equal? (get-field name (cdr place-type)) index))
              (ormap (lambda (x) 
                       ;; return x that covers the given range
                       (and (and (<= (get-field from x) (get-field from range))
                                (>= (get-field to x) (get-field to range)))
			    (get-field place x))) 
                     (car place-type))
              place-type)))
        
      (define (get-place-offset)
        (let ([place-type (get-field place-type ast)])
          (if (and (place-type-dist? place-type) 
                   (equal? (get-field name (cdr place-type)) index))
	      (let ([here (ormap (lambda (x) 
				   (and (<= (get-field from x) (get-field from range))
					(>= (get-field to x) (get-field to range))
					(get-field place x)))
				 (car place-type))]
		    [count 0])
		(for ([p (car place-type)])
		     (when (and (<= (get-field to p) (get-field from range))
				(not (= (get-field place p) here)))
			   (set! count (+ count (- (get-field to p) (get-field from p))))))
		(cons here count))
              (cons place-type 0))))
        
      (cond
       [(is-a? ast Const%)
        (new Const% [n (get-field n ast)] [place (get-field place ast)])]

       [(is-a? ast Num%)
        (new Num% [n (send (get-field n ast) accept this)] 
             [place-type (get-place-type)])
        ]

       [(is-a? ast Array%)
	(define place-offset (get-place-offset))
        (new Array% [name (get-field name ast)] 
             [index (send (get-field index ast) accept this)]
	     [offset (cdr place-offset)] ;; need this to substract from the index
	     [cluster (get-field cluster ast)]
             [place-type (car place-offset)] [known-type (get-known-type)])]

       [(is-a? ast Temp%)
        (new Temp% [name (get-field name ast)]
             [place-type (get-place-type)] [known-type (get-known-type)]
             [compact (get-field compact ast)])]
       

       [(is-a? ast Var%)
        (new Var% [name (get-field name ast)]
             [place-type (get-place-type)] [known-type (get-known-type)])]

       [(is-a? ast Op%)
        (new Op% [op (get-field op ast)] [place (get-field place ast)])]

       [(is-a? ast UnaExp%)
        (new UnaExp% 
             [op (send (get-field op ast) accept this)]
             [e1 (send (get-field e1 ast) accept this)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast BinExp%)
        (new BinExp% 
             [op (send (get-field op ast) accept this)]
             [e1 (send (get-field e1 ast) accept this)]
             [e2 (send (get-field e2 ast) accept this)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast FuncCall%)
        (new FuncCall%
             [name (get-field name ast)]
             [args (map (lambda (x) (send x accept this)) (get-field args ast))]
             ;[signature (send (get-field signature ast) accept this)]
             [signature (get-field signature ast)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast ProxyReturn%)
        (new ProxyReturn% [place-type (get-field place-type ast)])]

       [(is-a? ast Param%)
        (new Param%
             [var-list (get-field var-list ast)] ;; not copy
             [type (get-field type ast)]
             [known (get-field known ast)]
             [place (get-field place ast)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast VarDecl%)
        (new VarDecl%
             [var-list (get-field var-list ast)] ;; not copy
             [type (get-field type ast)]
             [known (get-field known ast)]
             [place (get-field place ast)])]
             
       [(is-a? ast ArrayDecl%)
        (new ArrayDecl%
             [var (get-field var ast)]
             [type (get-field type ast)]
             [known (get-field known ast)]
             [bound (get-field bound ast)]
             [init (get-field init ast)]
	     [cluster (get-field cluster ast)]
             [place-list (get-field place-list ast)])]

       [(is-a? ast For%)
        (new For%
             [iter (send (get-field iter ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [from (get-field from ast)]
             [to (get-field to ast)]
             [known (get-field known ast)]
             [place-list (get-field place-list ast)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [(is-a? ast If%)   
	(let ([false-block (get-field false-block ast)])
	  (get-new-if ast
		      (send (get-field condition ast) accept this)
		      (send (get-field true-block ast) accept this)
		      (and false-block (send false-block accept this))
		      (get-field body-placeset ast)))]

       [(is-a? ast While%)
	(get-new-while ast
		       (send (get-field condition ast) accept this)
		       (send (get-field body ast) accept this)
		       (get-field bound ast)
		       (get-field body-placeset ast)
		       (send (get-field pre ast) accept this))]

       [(is-a? ast AssignTemp%)
        (new AssignTemp%
             [lhs (send (get-field lhs ast) accept this)]
             [rhs (send (get-field rhs ast) accept this)])]

       [(is-a? ast Assign%)
        (new Assign%
             [lhs (send (get-field lhs ast) accept this)]
             [rhs (send (get-field rhs ast) accept this)])]

       [(is-a? ast Return%)
	(define val (get-field val ast))
        (new Return%
             [val (if (list? val) 
		      (map (lambda (x) (send x accept this)) val)
		      val)]
	     [expect (get-field expect ast)])]

       [(is-a? ast Block%)
        (new Block%
             [stmts (map (lambda (x) (send x accept this)) (get-field stmts ast))])]

       [(is-a? ast FuncDecl%)
        (new FuncDecl%
             [name (get-field name ast)]
             [args (send (get-field args ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [return (and (get-field return ast) 
                          (send (get-field return ast) accept this))]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [else
        (raise (format "visitor-cloner: unimplemented for ~a" ast))]

       ))))
             
