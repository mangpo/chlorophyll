#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(provide (all-defined-out))

(define arrayaccess%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [stack (list)] [index-stack 0] [uses-a #f] [port-usage #f])

    (define debug #f)

    (struct pack (iter arrays) #:mutable)

    (set! port-usage (make-hash)) ;;maps port addresses to usage count
    (define (add-port port)
      (hash-set! port-usage port (add1 (if (hash-has-key? port-usage port)
                                           (hash-ref port-usage port)
                                           0))))

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
        (when debug
              (pretty-display (format "\nACCESS: Array ~a" (send ast to-string))))
	(define index (get-field index ast))

        (if (and (not (empty? stack))
                 (= (get-field offset ast) 0)
		 (is-a? index Var%) 
		 (equal? (get-field name index) (pack-iter (car stack))))
	    (let ([top (car stack)])
	      (set-pack-arrays! top (cons ast (pack-arrays top))))
            (send index accept this))]

       [(is-a? ast Var%)
        (when debug
              (pretty-display (format "\nACCESS: Var ~a" (send ast to-string))))
        (define ele (findf (lambda (x) (equal? (pack-iter x) (get-field name ast))) stack))
        (when ele
              (set-pack-arrays! ele (append (list ast ast) (pack-arrays ele))))]

       [(or (is-a? ast VarDecl%)
            (is-a? ast ArrayDecl%)
            (is-a? ast Num%)
            (is-a? ast Recv%)
            (is-a? ast PortListen%)
            (is-a? ast PortExec%)
            )
        (when debug
          (pretty-display (format "\nACCESS: Decl, Num, Recv, PortListen, PortExec")))

        (when (is-a? ast Recv%)
          (add-port (get-field port ast)))
        0]
       
       [(is-a? ast UnaExp%)
        (when debug
              (pretty-display (format "\nACCESS: UnaExp ~a" (send ast to-string))))
        (send (get-field e1 ast) accept this)]
        
       [(is-a? ast BinExp%)
        (when debug
              (pretty-display (format "\nACCESS: BinExp ~a" (send ast to-string))))
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)]

       [(is-a? ast Send%)
        (when debug
              (pretty-display (format "\nACCESS: Send ~a" 
                                      (send (get-field data ast) to-string))))
        (send (get-field data ast) accept this)
        (add-port (get-field port ast))
        0]

       [(is-a? ast FuncCall%)
        (when debug
              (pretty-display (format "\nACCESS: FuncCall ~a" (send ast to-string))))
        (for ([x (get-field args ast)])
             (send x accept this))
        (define name (get-field name ast))
        (when (or (regexp-match #rx"set_io" name)
                  (regexp-match #rx"digital_read" name)
                  (regexp-match #rx"digital_wakeup" name)
                  (regexp-match #rx"delay_unext" name))
          (add-port 'IO))

        0]

       [(is-a? ast Assign%)
        (when debug
              (pretty-display (format "\nACCESS: Assign")))
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)
        0]

       [(is-a? ast Return%)
        (when debug
              (pretty-display (format "\nACCESS: Return")))

	(define val (get-field val ast))
	(if (list? val)
	    (for ([v val])
		 (send v accept this))
	    (send val accept this))
        0]

       [(is-a? ast If%)
        (when debug
              (pretty-display (format "\nACCESS: If ~a" 
                                      (send (get-field condition ast) to-string))))
        (send (get-field condition ast) accept this)
        (+ (send (get-field true-block ast) accept this)
           (if (get-field false-block ast)
               (send (get-field false-block ast) accept this)
               0))]

       [(is-a? ast While%)
        (when debug
              (pretty-display (format "\nACCESS: While ~a" 
                                      (send (get-field condition ast) to-string))))
	(send (get-field condition ast) accept this)
	(send (get-field pre ast) accept this)
	(send (get-field body ast) accept this)]

       [(is-a? ast For%)
        (when debug
              (pretty-display (format "\nACCESS: For (begin)")))
	(set! stack (cons (pack (get-field name (get-field iter ast)) (list))
                          stack))
        (define children-count (send (get-field body ast) accept this))
	(define arrays (pack-arrays (car stack)))
	(define my-count (length arrays))
        (when debug
              (pretty-display (format "\nACCESS: For (middle)"))
	      (pretty-display `(my-count ,my-count children-count ,children-count))
	      )

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
          (set! uses-a #t)
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
        (when debug
              (pretty-display (format "\nACCESS: FuncDecl ~a" (get-field name ast))))
	(send (get-field body ast) accept this)]
       
       [else
        (raise (format "visitor-arrayaccess: unimplemented for ~a" ast))]))))
      
