#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt")

(provide (all-defined-out))

(struct block (body in out) #:mutable)
(struct mult ()) ;; : mult (x y -> z) a! 0 17 for +* next drop drop a ;
(struct funccall (name))
(struct funcdecl (name body))
(struct forloop (body))

(define (codegen-print x [indent ""])
  (define (inc indent)
    (string-append indent "  "))
  (define (dec indent)
    (substring indent 2))

  (cond
   [(list? x)
    (for ([i x])
	 (codegen-print i))]
   [(block? x)
    (display (format "~a(block: " indent))
    (for ([i (block-body x)])
	 (display i)
	 (display " "))
    (pretty-display (format ", in:~a out:~a)" (block-in x) (block-out x)))]
   [(mult? x)
    (pretty-display (format "~a(mult)" indent))]
   [(funccall? x)
    (pretty-display (format "~a(funccall: ~a)"  indent (funccall-name x)))]
   [(forloop? x)
    (pretty-display (format "~a(for:"  indent))
    (codegen-print (forloop-body x) (inc indent))]
   [(funcdecl? x)
    (pretty-display (format "~a(funcdecl: ~a"  indent (funcdecl-name x)))
    (codegen-print (funcdecl-body x) (inc indent))]
   [else (raise (format "visitor-codegen: print: unimplemented for ~a" x))]))

(define code-generator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field data-size iter-size core w h 
		[x (floor (/ core w))] [y (modulo core w)])

    (define debug #t)

    (define-syntax gen-block
      (syntax-rules ()
	[(gen-block)
	 (block (list) 0 0)]
	[(gen-block a ... in out)
	 (block (list a ...) in out)]))

    (define-syntax prog-append
      (syntax-rules ()
	[(prog-append a b) (program-append a b)]
	[(prog-append a b c ...)
	 (prog-append (program-append a b) c ...)]))

    (define (is-temp? name)
      (regexp-match #rx"_temp" name))
      
    (define (gen-op op)
      (cond
       [(equal? op "~") (list (gen-block "-" 1 1))]
       [(equal? op "!") (list (gen-block "-" 1 1))]
       [(equal? op "*") (list (mult))]
       [(equal? op "-") (list (gen-block "-" "1" "." "+" "." "+" 2 1))]
       [(equal? op "+") (list (gen-block "." "+" 2 1))]
       [(equal? op ">>") (list (gen-block "dup" "dup" "or" "-" "." "+" 1 1) (forloop (list "2/")))] 
       ;; x-1 for 2/ unext
       [(equal? op "<<") (list (gen-block "dup" "dup" "or" "-" "." "+" 1 1) (forloop (list "2*")))] 
       ;; x-1 for 2* unext
       [(equal? op "&") (list (gen-block "and" 2 1))]
       [(equal? op "^") (list (gen-block "or" 2 1))]
       [(equal? op "|") (list (gen-block "over" "-" "and" "." "+" 2 1))]
       [else (raise (format "visitor-codegen: gen-op: unimplemented for ~a" op))]))

    (define (gen-port port)
      (cond
       [(equal? port `N)
	(if (= (modulo x 2) 0) "up" "down")]
       [(equal? port `S)
	(if (= (modulo x 2) 0) "down" "up")]
       [(equal? port `E)
	(if (= (modulo y 2) 0) "right" "left")]
       [(equal? port `W)
	(if (= (modulo y 2) 0) "left" "right")]))
	   
    (define (program-append a-list b-list)
      ;; merge b-block into a-block
      (define (merge-into a-block b-block)
	(set-block-body! a-block (append (block-body a-block) (block-body b-block)))
	(define a-in  (block-in a-block))
	(define a-out (block-out a-block))
	(define b-in  (block-in  b-block))
	(define b-out (block-out  b-block))
	(if (>= a-out b-in)
	    (set-block-out! a-block (- (+ a-out b-out) b-in))
	    (begin
	      (set-block-in! a-block (- (+ a-in b-in) a-out))
	      (set-block-out! a-block b-out))))

      (cond
       [(empty? a-list) b-list]
       [(empty? b-list) a-list]
       [else
	(define a-last (last a-list))
	(define b-first (car b-list))
	(if (and (block? a-last) (block? b-first))
	    (begin
	      (merge-into a-last b-first)
	      (append a-list (cdr b-list)))
	    (append a-list b-list))]))

    (define/public (visit ast)
      (cond
       [(or (is-a? ast VarDecl%)
	    (is-a? ast ArrayDecl%))
	(list)]

       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Num ~a" (send ast to-string))))
	(list (gen-block (number->string (get-field n (get-field n ast))) 0 1))]

       [(is-a? ast Array%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Array ~a" (send ast to-string))))
	(define index-ret (send (get-field index ast) accept this))
	(define address (get-field address ast))
	(define array-ret (gen-block (number->string (car address)) "." "+" "a!" "@" 1 1))
	(prog-append index-ret array-ret)]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Var ~a" (send ast to-string))))
	(define address (get-field address ast))
	(if address
	    ;; push on the stack
	    (if (cdr address)
		;; data
		(list (gen-block (number->string (car address)) "a!" "@" 0 1))
		;; iter
		(list (gen-block (number->string (+ data-size (car address))) "a!" "@" 0 1)))
	    ;; already on the stack
	    (list))]

       [(is-a? ast UnaExp%)
        (when debug 
              (pretty-display (format "\nCODEGEN: UnaExp ~a" (send ast to-string))))
	(define e1-ret (send (get-field e1 ast) accept this))
	(define op (get-field op (get-field op ast)))
	(prog-append e1-ret (gen-op op))]

       [(is-a? ast BinExp%)
        (when debug 
              (pretty-display (format "\nCODEGEN: BinExp ~a" (send ast to-string))))
	(define e1-ret (send (get-field e1 ast) accept this))
	(define e2-ret (send (get-field e2 ast) accept this))
	(define op (get-field op (get-field op ast)))
	(prog-append e1-ret e2-ret (gen-op op))]

       [(is-a? ast Recv%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Recv ~a" (get-field port ast))))
        (list (gen-block (gen-port (get-field port ast)) "a!" "@" 0 1))]

       [(is-a? ast Send%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Send ~a" (get-field port ast))))
	(define data-ret (send (get-field data ast) accept this))
	(define send-ret (list (gen-block (gen-port (get-field port ast)) "a!" "!" 1 0)))
        (prog-append data-ret send-ret)]

       [(is-a? ast FuncCall%)
        (when debug 
              (pretty-display (format "\nCODEGEN: FuncCall ~a" (send ast to-string))))
	;; (define args (get-field args ast))
	;; (define n-temp (count is-temp? args))
	;; (define move (block (for/list ([i n-temp]) "push") n-temp 0))
	;; (define arg-ret 
	;;   (foldl (lambda (arg all) (prog-append all (send arg accept this))) 
	;; 	 (list) args))
	(list (funccall (get-field name ast)))]

       [(is-a? ast Assign%)
	(define lhs (get-field lhs ast))
	(define rhs (get-field rhs ast))
        (when debug 
              (pretty-display (format "\nCODEGEN: Assign ~a = ~a" 
				      (send lhs to-string) (send rhs to-string))))
	(define address (get-field address lhs))
	(pretty-display `(address ,address))
	(if (is-a? lhs Array%)
	    (let ([index-ret (send (get-field index lhs) accept this)]
		  [rhs-ret (send rhs accept this)])
	      (prog-append 
	       index-ret
	       rhs-ret
	       (list (gen-block "over" (number->string (car address)) "." "+" "a!" "!" "drop" 2 0))))
	    (let ([rhs-ret (send rhs accept this)])
		  (prog-append
		   rhs-ret
		   (if address
		       (if (cdr address)
			   ;; data
			   (list (gen-block (number->string (car address)) "a!" "!" 1 0))
			   ;; iter
			   (list (gen-block (number->string (+ data-size (car address))) "a!" "!" 1 0)))
		       ;; temp on stack
		       (list)))))]

       [(is-a? ast Return%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Return")))
	(send (get-field data ast) accept this)]

       [(is-a? ast If%)
	;; TODO
	]
       
       [(is-a? ast While%)
	;; TODO
	]

       [(is-a? ast For%)
	;; TODO
	]

       [(is-a? ast FuncDecl%)
	(define decls (get-field stmts (get-field args ast)))
	(define n-decls (length decls))
	(define body-ret (send (get-field body ast) accept this))

	(if (> n-decls 0)
	    (let* ([address (get-field address (last decls))]
		   [args-ret (list
			      (block (append 
				      (list (number->string (car address)) "a!")
				      (for/list ([i (in-range n-decls)]) "!+")) n-decls 0))])
	      (funcdecl (get-field name ast) (prog-append args-ret body-ret)))
	    (funcdecl (get-field name ast) body-ret))]

       [(is-a? ast Program%)
	;; return list of function list
	(for/list ([decl (get-field stmts ast)])
		  (send decl accept this))]

       [(is-a? ast Block%)
	(foldl (lambda (stmt all) (prog-append all (send stmt accept this)))
	       (list) (get-field stmts ast))]

       [else
	(raise (format "visitor-codegen: unimplemented for ~a" ast))]

       ))))
