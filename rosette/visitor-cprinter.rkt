#lang s-exp rosette

(require "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define cprinter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field w h [core #f] [n (* w h)])

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
        (+ (* 2 n) port)]))

    (define/public (set-core c)
      (set! core c))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (display (format "~a ~a_~a;"
                               (get-field type ast)
                               (list-to-string (get-field var-list ast))
			       core
			       ))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a ~a_~a[~a];"
                               (get-field type ast)
                               (get-field var ast)
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
         (display (format "~a_~a[" (get-field name ast) core))
	 (send (get-field index ast) accept this)
	 (let ([offset (get-field offset ast)])
	   (when (> offset 0)
		 (display (format "-~a" offset))))
	 (display (format "]"))
         ]
      
        [(is-a? ast Var%)
	 (let ([name (get-field name ast)])
	   (if (equal? name "#return")
	       (display "return ")
	       (display (format "~a_~a" (get-field name ast) core))))
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
	   ;; (if (and (is-a? rhs FuncCall%) (equal? (get-field name rhs) "in"))
	   ;;     (begin
	   ;;       (display "scanf(\"%d\", &")
	   ;;       (send lhs accept this)
	   ;;       (display ")"))
	   ;;     (begin
           (send lhs accept this)
           (unless (equal? (get-field name lhs) "#return")
                   (display " = "))
           (send (get-field rhs ast) accept this))
	   ;; ))
         (display ";")
         ]

        [(is-a? ast If%)
         (display "if(")
         (send (get-field condition ast) accept this)
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

        [(is-a? ast FuncDecl%)
         (define (print-arg arg pre)
           (display (format "~a~a ~a_~a" pre
                           (get-field type arg) 
                           (car (get-field var-list arg))
			   core)))

	 (define name (get-field name ast))
         ;; Print function signature
	 (if (equal? name "main")
             ;; main
	     (pretty-display (format "void *main_~a(void *dummy) {" core))
             ;; everything else
	     (let* ([return (get-field return ast)]
		    [type (get-field type return)]
		    [place (send return get-place)])
	       (display (format "~a ~a_~a(" type name core))
	     
               ;; Print arguments
               (let ([arg-list (get-field stmts (get-field args ast))])
                 (when (not (empty? arg-list))
                       (print-arg (car arg-list) "")
                       (for ([arg (cdr arg-list)])
                            (print-arg arg ","))))

               (pretty-display ") {")))

         ;; Print Body
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
	       (pretty-display "return NULL;"))
         (dec-indent)
         (pretty-display "}")
         ]
        
        [else (raise "Error: printer unimplemented!")]
        
        ))))
