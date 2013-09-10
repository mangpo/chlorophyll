#lang racket

(require "header.rkt"
         "ast.rkt" 
	 "ast-util.rkt"
	 "visitor-interface.rkt"
         "arrayforth.rkt")

(provide (all-defined-out))

(define code-generator%
  (class* object% (visitor<%>)
    (super-new)
    (init-field data-size iter-size core w h virtual
		[x (floor (/ core w))] [y (modulo core w)]
                [helper-funcs (list)] 
                [data (make-vector (+ (meminfo-addr data-size) iter-size) 0)]
                [if-count 0] [while-count 0]
                [maxnum 1]
                ;; map virtual index to real index
                [index-map (make-hash)])

    (define debug #f)
    (define const-a (list #f))

    (define-syntax gen-block
      (syntax-rules ()
	[(gen-block)
	 (block (list) 0 0 (restrict #f #f #f #f) (list))]
	[(gen-block mem)
	 (block (list) 0 0 (restrict mem (car const-a) #f #f) (list))]
	[(gen-block a ... in out)
	 (block (list a ...) in out (restrict #t (car const-a) #f #f) (list a ...))]))

    (define-syntax gen-block-r
      (syntax-rules ()
	[(gen-block-r a ... in out)
         (block (list a ...) in out (restrict #t (car const-a) #f #t) (list a ...))]))
    
    (define-syntax gen-block-list
      (syntax-rules ()
	[(gen-block-list insts insts-org in out)
	 (block insts in out (restrict #t (car const-a) #f #f) insts-org)]))
    
    (define-syntax gen-block-org
      (syntax-rules ()
	[(gen-block-org (a ...) (b ...) in out)
	 (block (list a ...) in out (restrict #t (car const-a) #f #f) (list b ...))]))
    
    (define (gen-block-store addr addr-org in)
      (if (= in 1)
          (block (list addr "b!" "!b") in 0 (restrict #t (car const-a) #f #f) 
                 (list addr-org "b!" "!b"))
          (if (car const-a)
              (block (append (list "a" "push" addr "a!") 
                             (for/list ([i (in-range in)]) "!+")
                             (list "pop" "a!"))
                     in 0 (restrict #t (car const-a) #f #f)
                     (append (list "a" "push" addr-org "a!") 
                             (for/list ([i (in-range in)]) "!+")
                             (list "pop" "a!")))
              (block (append (list addr "a!") 
                             (for/list ([i (in-range in)]) "!+"))
                     in 0 (restrict #t (car const-a) #f #f)
                     (append (list addr-org "a!") 
                             (for/list ([i (in-range in)]) "!+"))))))
      
    (define (gen-op op)
      (cond
       [(equal? op "~") (list (gen-block "-" 1 1))]
       [(equal? op "!") (list (gen-block "-" 1 1))]
       [(equal? op "*") 
        (if (car const-a)
            (list (gen-block-r "a" "push" 0 0) (mult) (gen-block "pop" "a!" 0 0))
            (list (mult)))]

       [(equal? op "-") (list (gen-block "-" "1" "+" "+" 2 1))]
       [(equal? op "+") (list (gen-block "+" 2 1))]
       [(equal? op ">>") ;; x-1 for 2/ unext
        (list (ift (list (gen-block "-1" "+" 1 1) 
                         (forloop (gen-block) (list (gen-block "2/" 1 1)) #f #f #f)
                         (gen-block "dup" 1 2)))
              (gen-block "drop" 1 0))]

       [(equal? op "<<") ;; x-1 for 2* unext
        (list (ift (list (gen-block "-1" "+" 1 1) 
                         (forloop (gen-block) (list (gen-block "2*" 1 1)) #f #f #f)
                         (gen-block "dup" 1 2)))
              (gen-block "drop" 1 0))]

       [(equal? op "&") (list (gen-block "and" 2 1))]
       [(equal? op "^") (list (gen-block "or" 2 1))]
       [(equal? op "|") (list (gen-block "over" "-" "and" "+" 2 1))]
       [else (raise (format "visitor-codegen: gen-op: unimplemented for ~a" op))]))

    (define (gen-port port)
      ;(pretty-display `(gen-port ,port))
      (cond
       [(equal? port `N)
	(if (= (modulo x 2) 0) "up" "down")]
       [(equal? port `S)
	(if (= (modulo x 2) 0) "down" "up")]
       [(equal? port `E)
	(if (= (modulo y 2) 0) "right" "left")]
       [(equal? port `W)
	(if (= (modulo y 2) 0) "left" "right")]
       [(equal? port `IO)
        "io"]))

    (define (get-if-name)
      (set! if-count (add1 if-count))
      (format "~aif" if-count))

    (define (get-while-name)
      (set! while-count (add1 while-count))
      (format "~awhile" while-count))

    (define (define-if body)
      (define name (get-if-name))
      (define new-if (funcdecl name body))
      (set! helper-funcs (cons new-if helper-funcs))
      (list (funccall name)))

    (define (get-op exp)
      (get-field op (get-field op exp)))

    (define (get-e1 exp)
      (get-field e1 exp))

    (define (get-e2 exp)
      (get-field e2 exp))

    (define (binop-equal? exp str)
      (and (is-a? exp BinExp%) (equal? (get-op exp) str)))
    
    (define (minus e1 e2)
      (if (and (is-a? e2 Num%) (= 0 (get-field n (get-field n e2))))
	  e1
	  (new BinExp% [op (new Op% [op "-"])] [e1 e1] [e2 e2])))

    (define (get-var mem)
      ;(pretty-display `(index-map ,(meminfo-virtual mem) ,(meminfo-addr mem)))
      (dict-set! index-map (meminfo-virtual mem) (meminfo-addr mem))
      (if virtual (meminfo-virtual mem) (meminfo-addr mem)))

    (define (get-iter mem)
      (define reduce (+ (meminfo-virtual data-size) (meminfo-virtual mem)))
      (define actual (+ (meminfo-addr data-size) (meminfo-virtual mem)))
      (dict-set! index-map reduce actual)
      (if virtual reduce actual))

    (define (get-iter-org mem)
      (+ (meminfo-addr data-size) (meminfo-addr mem)))

    (define/public (visit ast)
      (cond
       [(is-a? ast VarDecl%)
        (list)]

       [(is-a? ast ArrayDecl%)
        (when debug
              (pretty-display (format "\nCODEGEN: VarDecl ~a" (get-field var ast))))
        (define init (get-field init ast))
        (when init ;(and (not virtual) init)
              (define address (get-field address ast))
              (for ([i (in-range (length init))]
                    [val init])
                   (vector-set! data (+ (meminfo-addr address) i) val)))

        (when debug
              (pretty-display (format "\nCODEGEN: VarDecl ~a (done)" (get-field var ast))))
        (list)
        ]

       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Num ~a" (send ast to-string))))
        (define n (get-field n (get-field n ast)))
        (when (> n maxnum)
              (set! maxnum n))
	(list (gen-block (number->string n) 0 1))]

       [(is-a? ast Array%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Array ~a" (send ast to-string))))
	(define index-ret (send (get-field index ast) accept this))

	(define offset (get-field offset ast))
	(define address (get-field address ast))
	(define opt (get-field opt ast))

        (define insts
          (if opt
              (list "@+")
              (let ([actual-addr (- (get-var address) offset)])
                (if (= actual-addr 0)
                    (list "b!" "@b")
                    (list (number->string actual-addr) "+" "b!" "@b")))))
        (define insts-org
          (if opt
              (list "@+")
              (let ([actual-addr-org (- (meminfo-addr address) offset)])
                (if (= actual-addr-org 0)
                    (list "b!" "@b")
                    (list (number->string actual-addr-org) "+" "b!" "@b")))))

        (define array-ret
          (list (gen-block-list insts insts-org 1 1)))

        (if opt
            array-ret
            (prog-append index-ret array-ret))
        ]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Var ~a" (send ast to-string))))
	(define address (get-field address ast))
	(if address
	    ;; push on the stack
	    (if (meminfo-data address)
		;; data
		(list (gen-block-org 
                       ((number->string (get-var address)) "b!" "@b")
                       ((number->string (meminfo-addr address)) "b!" "@b")
                       0 1
                       ))
		;; iter
		(list (gen-block-org 
                       ((number->string (get-iter address)) "b!" "@b")
                       ((number->string (get-iter-org address)) "b!" "@b")
                       0 1
                       )))
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
        (list (gen-block (gen-port (get-field port ast)) "b!" "@b" 0 1))]

       [(is-a? ast Send%)
        (define data (get-field data ast))
        (when debug 
              (pretty-display (format "\nCODEGEN: Send ~a ~a" (get-field port ast) data)))
	(define data-ret (send data accept this))
        (define temp-ret
          (if (and (is-a? data Temp%) (equal? (get-field name data) "_cond"))
              (list (gen-block "dup" 1 2))
              (list (gen-block))))
	(define send-ret (list (gen-block (gen-port (get-field port ast)) "b!" "!b" 1 0)))
        (prog-append data-ret temp-ret send-ret)]

       [(is-a? ast FuncCall%)
        (when debug 
              (pretty-display (format "\nCODEGEN: FuncCall ~a" (send ast to-string))))
        (define arg-code 
          (foldl (lambda (x all) (prog-append all (send x accept this)))
                 (list) (get-field args ast)))
        (define call-code
          (if (car const-a)
              (list (gen-block-r "a" "push" 0 0)
                    (funccall (get-field name ast))
                    (gen-block "pop" "a!" 0 0))
              (list (funccall (get-field name ast)))))
        (prog-append arg-code call-code)
        ]

       [(is-a? ast Assign%)
	(define lhs (get-field lhs ast))
	(define rhs (get-field rhs ast))
        (when debug 
              (pretty-display (format "\nCODEGEN: Assign ~a = ~a" 
				      (send lhs to-string) (send rhs to-string))))
	(define address (get-field address lhs))
	;(pretty-display `(address ,address))
	(if (is-a? lhs Array%)
	    (let* ([index-ret (send (get-field index lhs) accept this)]
		   [offset    (get-field offset lhs)]
		   [rhs-ret     (send rhs accept this)]
                   [actual-addr (- (get-var address) offset)]
                   [actual-addr-org (- (meminfo-addr address) offset)]
		   [opt (get-field opt lhs)]
                   [insts 
                    (if opt
                        (list "!+")
                        (if (= actual-addr 0)
                            (list "b!" "!b")
                            (list (number->string actual-addr) "+" "b!" "!b")))]
                   [insts-org 
                    (if opt
                        (list "!+")
                        (if (= actual-addr-org 0)
                            (list "b!" "!b")
                            (list (number->string actual-addr-org) "+" "b!" "!b")))])
              (if opt
                  (prog-append rhs-ret (list (gen-block-list insts insts-org 1 0)))
                  (prog-append rhs-ret index-ret (list (gen-block-list insts insts-org 2 0)))))
	    (let ([rhs-ret (send rhs accept this)])
		  (prog-append
		   rhs-ret
		   (if address
		       (if (meminfo-data address)
			   ;; data
                           (if (pair? (get-field type lhs))
                               ;; need to expand
                               (list 
                                (gen-block-store (number->string (get-var address))
                                                 (number->string (meminfo-addr address))
                                                 (cdr (get-field type lhs))))
                               (list
                                (gen-block-store (number->string (get-var address))
                                                 (number->string (meminfo-addr address))
                                                 1)))
			   ;; iter
			   (list (gen-block-org
                                  ((number->string (get-iter address)) "b!" "!b")
                                  ((number->string (get-iter-org address)) "b!" "!b")
                                  1 0
                                  )))
		       ;; temp on stack
		       (list)))))]

       [(is-a? ast Return%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Return")))
        (define val (get-field val ast))
	(define ret
	  (if (list? val)
	      (foldl (lambda (v all) (prog-append all (send v accept this)))
		     (list) val)
	      (send (get-field val ast) accept this)))

	(if (empty? ret)
	    (list (gen-block #f))
	    (begin
	      (set-restrict-mem! (block-cnstr (last ret)) #f)
	      ret))
	]

       [(is-a? ast If%)
	;; not yet support && ||
        (when debug 
              (pretty-display (format "\nCODEGEN: If"))
	      (pretty-display ">>> pre")
	      (send (get-field pre ast) pretty-print)
	      (pretty-display "<<<")
	      )
        (define pre-ret 
	  (if (get-field pre ast)
	      (send (get-field pre ast) accept this)
	      (list (gen-block))))
	  
	(codegen-print pre-ret)
        (define cond-ret (send (get-field condition ast) accept this))
        (define true-ret (send (get-field true-block ast) accept this))
        (define false-ret 
          (if (get-field false-block ast)
              (send (get-field false-block ast) accept this)
              #f))

        (cond
         [(is-a? ast If!=0%)
          (if false-ret
              (define-if (prog-append pre-ret cond-ret (list (iftf true-ret false-ret))))
              (prog-append pre-ret cond-ret (list (ift true-ret))))
          ]

         [(is-a? ast If<0%)
          (if false-ret
              (define-if (prog-append pre-ret cond-ret (list (-iftf true-ret false-ret))))
              (prog-append pre-ret cond-ret (list (-ift true-ret))))
          ]

         [else
          (if false-ret
              (define-if (prog-append pre-ret cond-ret (list (iftf true-ret false-ret))))
              (prog-append pre-ret cond-ret (list (ift true-ret))))])]
       
       [(is-a? ast While%)
	(define pre (get-field pre ast))
        (when debug 
              (pretty-display (format "\nCODEGEN: While"))
	      (pretty-display ">>> pre")
	      (send pre pretty-print)
	      (pretty-display "<<<")
	      )
	(define exp (get-field condition ast))
	(define name (get-while-name))
	(define body (get-field body ast))
	(define block (new Block% [stmts (append (get-field stmts body)
						 (list (new FuncCall% [name name] [args (list)])))]))
        (define empty-block (new Block% [stmts (list)]))

	;; desugar into if construct
	;; set name = while-name
	(define if-rep
	  (cond
	   [(is-a? ast While!=0%) 
	    (new If!=0% [pre pre] [condition exp] 
                 [true-block block] [false-block empty-block])]

	   [(is-a? ast While==0%)
	    (new If!=0% [pre pre] [condition exp] 
		 [true-block empty-block] [false-block block])]

	   [(is-a? ast While<0%)
	    (new If<0% [pre pre] [condition exp] 
                 [true-block block] [false-block empty-block])]
	    
	   [(is-a? ast While>=0%) 
	    (new If<0% [pre pre] [condition exp] 
		 [true-block empty-block] [false-block block])]

	   [else
	    (new If% [pre pre] [condition exp] 
                 [true-block block] [false-block empty-block])]))
	
	(define if-ret (send if-rep accept this))
	;; (pretty-display "~~~~~~~~~~~~~~~~~~~~~~")
	;; (pretty-display "AST")
	;; (send if-rep pretty-print)

	;; (pretty-display "RESULT")
	;; (codegen-print if-ret)
	;; (pretty-display "~~~~~~~~~~~~~~~~~~~~~~")

	(unless (funccall? (car if-ret))
		(define-if if-ret))

        ;; rename last function declaration to while-name
        (set-funcdecl-name! (car helper-funcs) name)

	(list (funccall name))
	]

       [(is-a? ast For%)
	(define array (get-field iter-type ast))

        (define from (get-field from ast))
        (define to (get-field to ast))
        (define addr-pair #f)
        
        ;; if no arrayaccess => no need to initialize
        (define init-ret
          (cond
           [(equal? array 0)
	    ;; same restriction on a
	    (set! const-a (cons (car const-a) const-a))
	    (set! addr-pair (cons #f #f))
            (gen-block (number->string (- to from 1)) 0 1)
            ]
           
           [(is-a? array Array%)
	    ;; constraint a
	    (set! const-a (cons #t const-a))
            (define offset (get-field offset array))
            (define address (get-field address array))
            (define actual-addr-str (number->string (- (get-var address) offset)))
            (define actual-addr-org-str (number->string (- (meminfo-addr address) offset)))
	    (set! addr-pair (cons (cons actual-addr-str `opt)
				  (cons actual-addr-org-str `opt)))
            (gen-block-org
             (actual-addr-str "a!" (number->string (- to from 1)))
             (actual-addr-org-str "a!" (number->string (- to from 1)))
             0 1)
            ]

           [else
	    ;; same restriction on a
            (define address (get-iter (get-field address ast)))
            (define address-str (number->string address))
            (define address-org (get-iter-org (get-field address ast)))
            (define address-org-str (number->string address-org))

	    (set! const-a (cons (car const-a) const-a))
	    (set! addr-pair (cons address-str address-org-str))
            (gen-block-org
             ((number->string from) address-str 
              "b!" "!b" (number->string (- to from 1)))
             ((number->string from) address-org-str
              "b!" "!b" (number->string (- to from 1)))
             0 1)
            ])) ;; loop bound
         
        (define body-ret (send (get-field body ast) accept this))
	;; pop restriction on a
	(set! const-a (cdr const-a))

        (define body-decor 
          (list (if (or (equal? array 0) (is-a? array Array%))
                    (gen-block)
                    (let* ([address (get-iter (get-field address ast))]
                           [address-str (number->string address)]
                           [address-org (get-iter-org (get-field address ast))]
                           [address-org-str (number->string address-org)])
                      (gen-block-org 
                       (address-str "b!" "@b" "1" "+" "!b")
                       (address-org-str "b!" "@b" "1" "+" "!b")
                       0 0)))))

        (list (forloop init-ret (prog-append body-ret body-decor) 
                       addr-pair from to))
	]

       [(is-a? ast FuncDecl%)
	(define decls (get-field stmts (get-field args ast)))
	(define n-decls (length decls))
	(define body-ret (send (get-field body ast) accept this))

	(if (> n-decls 0)
	    (let* ([address (get-field address (last decls))]
                   [code (append (list (number->string (get-var address)) "a!")
                                 (for/list ([i (in-range n-decls)]) "!+"))]
		   [args-ret (list (gen-block-list code code n-decls 0))])
	      (funcdecl (get-field name ast) (prog-append args-ret body-ret)))
	    (funcdecl (get-field name ast) body-ret))]

       [(is-a? ast Program%)
        (when debug 
              (pretty-display (format "\nCODEGEN: Program")))

        (if (empty? (get-field stmts ast))
            #f
            
            ;; return list of function list
            (let ([main-funcs
                   (for/list ([decl (filter (lambda (x) (is-a? x FuncDecl%)) 
                                            (get-field stmts ast))])
                             (send decl accept this))])
              
              (dict-set! index-map 
                         (+ (meminfo-virtual data-size) iter-size)
                         (+ (meminfo-addr data-size) iter-size))
              (aforth (append (list (vardecl (vector->list data))) 
                              (reverse helper-funcs) 
                              main-funcs) 
                      (+ (get-var data-size) iter-size) 
                      (max (inexact->exact (floor (+ (/ (log maxnum) (log 2)) 2))) ga-bit)
                      (if virtual index-map #f))))
        ]

       [(is-a? ast Block%)
	(foldl (lambda (stmt all) (prog-append all (send stmt accept this)))
	       (list) (get-field stmts ast))]

       [else
	(raise (format "visitor-codegen: unimplemented for ~a" ast))]

       ))))
