#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" 
         "visitor-interface.rkt")

(provide (all-defined-out))

(define funccall-linker%
  (class* object% (visitor<%>)
    (super-new)
    (define funcdecls (make-hash))

    (hash-set! funcdecls "in" (get-stdin))
    (hash-set! funcdecls "out" (get-stdout))
    (for ([node digital-nodes])
      (hash-set! funcdecls
		 (format "digital_write~a" node) (get-digital-write node))
      (hash-set! funcdecls
		 (format "digital_read~a" node) (get-digital-read node)))

    (for ([node (append analog-nodes digital-nodes)])
      (hash-set! funcdecls
		 (format "digital_wakeup~a" node) (get-digital-wakeup node))
      (hash-set! funcdecls
		 (format "delay_ns~a" node) (get-delay-ns node))
      (hash-set! funcdecls
		 (format "delay_unext~a" node) (get-delay-unext node)))

    (define/public (visit ast)
      (cond
        [(is-a? ast UnaExp%)
	 ;; (pretty-display (format "FLINK: UnaExp ~a" (send ast to-string)))
         (send (get-field e1 ast) accept this)]

        [(is-a? ast BinExp%)
	 ;; (pretty-display (format "FLINK: BinExp ~a" (send ast to-string)))
         (send (get-field e1 ast) accept this)
         (send (get-field e2 ast) accept this)]

        [(is-a? ast FuncCall%)
	 ;; (pretty-display (format "FLINK: FuncCall ~a, sig=~a" (get-field name ast)
	 ;; 			 (hash-ref funcdecls (get-field name ast))))
         (set-field! signature ast (hash-ref funcdecls (get-field name ast)))
         (for ([arg (get-field args ast)])
              (send arg accept this))
         ]

        [(is-a? ast Send%)
	 (send (get-field data ast) accept this)]

        [(is-a? ast Assign%)
	 ;; (pretty-display (format "FLINK: Assign"))
         (send (get-field lhs ast) accept this)
         (send (get-field rhs ast) accept this)]

        [(is-a? ast Return%)
         (define val (get-field val ast))
         (if (list? val)
             (for ([v val])
               (send v accept this))
             (send val accept this))]

        [(is-a? ast If%)
         (send (get-field condition ast) accept this)
         (send (get-field true-block ast) accept this)
         (when (get-field false-block ast)
               (send (get-field false-block ast) accept this))]

        [(is-a? ast While%)
         (send (get-field pre ast) accept this)
         (send (get-field condition ast) accept this)
         (send (get-field body ast) accept this)]

        [(is-a? ast For%)
	 ;; (pretty-display (format "FLINK: For"))
         (send (get-field body ast) accept this)
	 ]

        [(is-a? ast VarDeclDup%)
	 (define old-loop (get-field loop ast))
	 (set-field! loop ast (get-field myclone old-loop))
	 ;; (pretty-display (format "FLINK: Dup ~a, old-loop = ~a, loop=~a" ast old-loop 
	 ;; 			 (get-field myclone old-loop)))
	 ]
	 
        [(is-a? ast Block%)
	 (when (is-a? ast BlockDup%)
	       (define old-loop (get-field loop ast))
	       (set-field! loop ast (get-field myclone old-loop)))
         (for ([stmt (get-field stmts ast)])
              (send stmt accept this))]

        [(is-a? ast FuncDecl%)
	 ;; (pretty-display (format "FLINK: FuncDecl ~a" (get-field name ast)))
         (hash-set! funcdecls (get-field name ast) ast)
         (send (get-field body ast) accept this)]
        ))))
         
        
(define (clone x)
  (define ret (send x clone))
  (send ret accept (new funccall-linker%))
  ret)
         
