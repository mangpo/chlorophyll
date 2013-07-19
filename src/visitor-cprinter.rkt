#lang s-exp rosette

(require "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define cprinter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field thread [w 0] [h 0] [core 0] [n (* w h)] [expand #f] [print-return #f])

    (define indent "")

    (define (inc-indent)
      (set! indent (string-append indent "  ")))

    (define (dec-indent)
      (set! indent (substring indent 2)))

    (define (channel core port)
      (cond
       [(equal? port `S)
	core]
       
       [(equal? port `N)
	(- core w)]
       
       [(equal? port `E)
	(+ n core)]

       [(equal? port `W)
	(sub1 (+ n core))]

       [(equal? port `IO)
        (+ (* 2 n) core)]

       [(number? port)
        (+ (* 2 n) port)]
       
       ;; use an n by n table for interfilter communication
       [(is-a? port ConcreteFilterDecl%)
        (define port-id (get-field id port))
        (define small (min core port-id))
        (define large (max core port-id))
        (+ (* 3 n) (* small n) (large))
        ]))

    (define (print-type type)
      (if (string? type)
	  type
	  (format "~a~a" (car type) (cdr type))))

    (define (print-name name)
      (regexp-replace* #rx"#" (regexp-replace* #rx"::" name "_") "_"))

    (define print-sub (vector "fst" "snd" "thd" "frth" "ffth"))

    (define (is-return? ast)
      (and (equal? (get-field name ast) "#return") (not (get-field sub ast))))

    (define/public (set-core c)
      (set! core c))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (display (format "~a ~a;"
                               (print-type (get-field type ast))
                               (list-to-string (map print-name (get-field var-list ast))
                                               (if thread core #f))
			       ))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a ~a_~a[~a];"
                               (get-field type ast)
                               (print-name (get-field var ast))
			       core
			       (get-field bound ast)))
         ]

        [(is-a? ast Const%)
         (display (send ast to-string))]
         
        
        
        [(is-a? ast Op%)
         (display (get-field op ast))
         ]


        [(is-a? ast Num%)
	 (display (get-field n (get-field n ast)))
         ]
      
        [(is-a? ast Array%)
         (display (format "~a_~a[" (print-name (get-field name ast)) core))
	 (send (get-field index ast) accept this)
	 (let ([offset (get-field offset ast)])
	   (when (> offset 0)
		 (display (format "-~a" offset))))
	 (display (format "]"))
         ]
      
        [(is-a? ast Var%)
	 (define name (get-field name ast))
	 (define sub (get-field sub ast))

	 (when (not thread)
	       ;; this renaming is only relavent for sequential version
	       (set! sub #f)
	       (when (or (regexp-match #rx"_temp" name) (regexp-match #rx"#return" name))
		     (let ([full-name (regexp-match #rx"(.+)::(.+)" name)])
		       (when full-name
			     ;; "a::0" -> ("a::0" "a" "0")
			     (let* ([actual-name (cadr full-name)]
				    [expand (string->number (caddr full-name))])
			       (set! name actual-name)
			       (set! sub expand))))))

	 ;; (when (and (equal? name "#return")
	 ;; 	    sub (= (add1 sub) expand))
	 ;;       (set! print-return #t))

	 ;; (if (is-return? ast)
	 ;;     (display "return ")
	 ;;     (begin
	 ;;       (display (format "~a_~a" (print-name name) core))
	 ;;       (when sub
	 ;; 	     (display (format ".~a" (vector-ref print-sub sub))))))

         (if thread
           (display (format "~a_~a" (print-name name) core))
           (display (print-name name)))

	 (when sub
	       (display (format ".~a" (vector-ref print-sub sub))))
         ]
        
        [(is-a? ast UnaExp%)
         (display "(")
	 (display (send (get-field op ast) to-string))
         (send (get-field e1 ast) accept this)
         (display ")")
         ]
        
        [(is-a? ast BinExp%)
         (display "(")
         (send (get-field e1 ast) accept this)
	 (display (send (get-field op ast) to-string))
         (send (get-field e2 ast) accept this)
         (display ")")
         ]

	[(is-a? ast FuncCall%)
	 (define name (get-field name ast))
	 (define args (get-field args ast))
	 ;; (cond
	 ;;  [(equal? name "out")
	 ;;   (display "printf(\"%d\\n\", ")
	 ;;   (send (car args) accept this)
	 ;;   (display ")")]

	 ;;  [else
         (if (or (equal? name "out") (equal? name "in"))
             (display (format "~a(" name))
             (display (format "~a_~a(" name core)))
         (unless (empty? args)
                 (send (car args) accept this)
                 (for ([arg (cdr args)])
                      (display ", ")
                      (send arg accept this)))
         (display ")")]
        ;; )]

	[(is-a? ast Recv%)
	 (display (format "read(~a)" (channel core (get-field port ast))))]

	[(is-a? ast Send%)
	 (display (format "write(~a," (channel core (get-field port ast))))
	 (send (get-field data ast) accept this)
	 (display ");")]
        
        [(is-a? ast Assign%)
	 (let ([lhs (get-field lhs ast)]
	       [rhs (get-field rhs ast)])
           (send lhs accept this)
           ;; (unless (is-return? lhs)
           ;;         (display " = "))
           (display " = ")
           (send (get-field rhs ast) accept this)
	   (display ";"))

	 ;; (when print-return
	 ;;       (set! print-return #f)
	 ;;       (newline)
	 ;;       (display indent)
	 ;;       (display (format "return _return_~a;" core)))
         ]

        [(is-a? ast Return%)
         (display "return ")
         (let ([val (get-field val ast)])
           (if (list? val)
               (begin
                 (display (format "int~a(" (length val)))
                 (send (car val) accept this)
                 (for ([x (cdr val)])
                      (display ", ")
                      (send x accept this))
                 (display ")"))
               (send val accept this)))
         (display ";")]

        [(is-a? ast If%)
         (display "if(")
         (send (get-field condition ast) accept this)

         (when (is-a? ast If<0%) (display " < 0"))

         (pretty-display ") {")
         (inc-indent)
         (send (get-field true-block ast) accept this)
         (dec-indent)
         (display (format "~a}" indent))
         (when (get-field false-block ast)
               (pretty-display " else {")
               (inc-indent)
               (send (get-field false-block ast) accept this)
               (dec-indent)
               (pretty-display (format "~a}" indent)))
         ]

	[(is-a? ast While%)
	 (display "while(")
	 (send (get-field condition ast) accept this)

         (cond
          [(is-a? ast While==0%) (display " == 0")]
          [(is-a? ast While<0%)  (display " < 0")]
          [(is-a? ast While>=0%) (display " > 0")])

	 (pretty-display ") {")
         (inc-indent)
         (send (get-field body ast) accept this)
         (dec-indent)
         (display (format "~a}" indent))]

        [(is-a? ast For%)
         (let ([iter (send (get-field iter ast) to-string)])
           (pretty-display (format "for(int ~a_~a = ~a; ~a_~a < ~a; ++~a_~a) {"
                                   iter core (get-field from ast)
                                   iter core (get-field to ast)
                                   iter core)))
	 (inc-indent)
	 (send (get-field body ast) accept this)
	 (dec-indent)
	 (pretty-display (format "~a}" indent))
         ]
        
        [(is-a? ast Block%)
         (for ([stmt (get-field stmts ast)])
	   (display indent)
           (send stmt accept this)
	   (when (is-a? stmt Exp%)
		 (display ";"))
           (newline))]

        [(is-a? ast RuntimeCallableDecl%)
         (define (print-arg arg pre)
           (display (format "~a~a ~a_~a" pre
                           (get-field type arg) 
                           (print-name (car (get-field var-list arg)))
			   core)))

         (define name
           (cond [(is-a? ast FuncDecl%) (get-field name ast)]
                 [(is-a? ast ConcreteFilterDecl%) "main"]))
         (define type
           (cond [(is-a? ast FuncDecl%) (get-field type (get-field return ast))]
                 [(is-a? ast ConcreteFilterDecl%) "void"]))
         (set! expand #f)
         
         ;; Print function signature
	 (if (equal? name "main")
             ;; main
             (pretty-display (format "void *main_~a(void *dummy) {" core))
             ;; everything else
	     (begin
	       (display (format "~a ~a_~a(" (print-type type) name core))
	     
               ;; Print arguments
               (let ([arg-list (get-field stmts (get-field args ast))])
                 (when (not (empty? arg-list))
                       (print-arg (car arg-list) "")
                       (for ([arg (cdr arg-list)])
                            (print-arg arg ","))))

               (pretty-display ") {")
	       (inc-indent)
	       
	       ;; Declare return variable
	       (display indent)
	       (cond 
		[(pair? type)
		 (set! expand (cdr type)) ;; TODO: get rid of this
		 (pretty-display (format "~a _return_~a;" (print-type type) core))]
		[(not (equal? type "void"))
		 (pretty-display (format "~a _return_~a;" type core))])

	       (dec-indent)
	       ))

         (inc-indent)

         ;; Declare temps
         (display indent)
         (display (format "int _tmp_~a" core))
         (for ([temp (get-field temps ast)])
              (display (format ", ~a_~a" temp core)))
         (pretty-display ";")

         ;; Body
         (send (get-field body ast) accept this)
	 (when (equal? name "main")
	       (display indent)
               (pretty-display "return NULL;")
               )
         (dec-indent)
         (pretty-display "}")
         ]
        
        [else (raise (format "visitor-cprinter: unimplemented for ~a" ast))]
        
        ))))
