#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(provide (all-defined-out))

(define arrayaccess%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [stack (list)] [index-stack 0])

    (struct pack (iter arrays) #:mutable)

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
	(define index (get-field index ast))
	;; (define ele (and (is-a? index Var%) 
	;; 		  (findf (lambda (x) 
	;; 			   (pretty-display (format "~a vs ~a" (pack-iter x) (get-field name index)))
	;; 			   (equal? (pack-iter x) (get-field name index))) stack)))
	;; (pretty-display ele)
	;; (when ele
	;;       (if (equal? ele (car stack))
	;; 	  ;; add 1 array
	;; 	  (set-pack-arrays! ele (cons ast (pack-arrays ele)))
	;; 	  ;; add 2 arrays (to exceed the optimization limit
	;; 	  (set-pack-arrays! ele (append (list ast ast) (pack-arrays ele)))))

        (define top (car stack))
        (if (and (is-a? index Var%) (equal? (get-field name index) (pack-iter top)))
            (set-pack-arrays! top (cons ast (pack-arrays top)))
            (begin
              (set! index-stack (add1 index-stack))
              (send index accept this)
              (set! index-stack (sub1 index-stack))))]

       [(is-a? ast Var%)
        (define ele (findf (lambda (x) (equal? (pack-iter x) (get-field name ast))) stack))
        (when ele
              (set-pack-arrays! ele (append (list ast ast) (pack-arrays ele))))]

       [(or (is-a? ast VarDecl%)
            (is-a? ast ArrayDecl%)
            (is-a? ast Num%)
            (is-a? ast Recv%))
        0]
       
       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)]
        
       [(is-a? ast BinExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)]

       [(is-a? ast Send%)
        (send (get-field data ast) accept this)
        0]

       [(is-a? ast FuncCall%)
        (for ([x (get-field args ast)])
             (send x accept this))
        0]

       [(is-a? ast Assign%)
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)
        0]

       [(is-a? ast Return%)
        (send (get-field val ast) accept this)
        0]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (+ (send (get-field true-block ast) accept this)
           (if (get-field false-block ast)
               (send (get-field false-block ast) accept this)
               0))]

       [(is-a? ast While%)
        (+ (send (get-field pre ast) accept this)
	   (send (get-field condition ast) accept this)
           (send (get-field body ast) accept this))]

       [(is-a? ast For%)
	(set! stack (cons (pack (get-field name (get-field iter ast)) (list))
                          stack))
        (define children-count (send (get-field body ast) accept this))
	(define arrays (pack-arrays (car stack)))
	(define my-count (length arrays))

	(set! stack (cdr stack))
        (cond 
	 [(= my-count 0)
	  (set-field! iter-type ast 0)
	  children-count
	  ]

	 [(and (= my-count 1) (= children-count 0))
	  (define array (car arrays))
	  (set-field! opt array #t)
	  (set-field! iter-type ast array)
	  1
	  ]

	 [else
	  (set-field! iter-type ast (+ my-count children-count))
	  children-count
	  ])
	]

       [(is-a? ast Block%)
	(foldl (lambda (stmt all) (+ all (send stmt accept this)))
	       0 (get-field stmts ast))]

       [(is-a? ast FuncDecl%)
	(send (get-field body ast) accept this)]
       
       [else
        (raise (format "visitor-arrayaccess: unimplemented for ~a" ast))]))))
      
