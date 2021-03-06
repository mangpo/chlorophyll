#lang s-exp rosette

(require "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define printer%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [out #f])

    (define indent "")

    (define (inc-indent)
      (set! indent (string-append indent "  ")))

    (define (dec-indent)
      (set! indent (substring indent 2)))

    (define (print-type x)
      (cond
       [(string? x) x]
       [(fix_t? x) (format "fix~a_t" (fix_t-int x))]
       [(pair? x) (format "~a::~a" (print-type (car x)) (cdr x))]
       [else (raise (format "visit-printer: print-type: unimplemented for ~a" x))]))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (display (format "~a@~a ~a;"
                               (print-type (get-field type ast))
                               (place-to-string (send ast get-place) out)
                               (list-to-string (get-field var-list ast))
			       ))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a[]@~a ~a[~a]"
                               (print-type (get-field type ast))
                               (place-to-string (get-field place-list ast) out)
                               (get-field var ast)
			       (get-field bound ast)))
         (define init (get-field init ast))
         (when init
               (display " = {")
               (display-list init)
               (display "}"))
         (display ";")
         ]

        [(is-a? ast Const%)
         (display (format "~a@~a"
                        (send ast to-string)
                        (place-to-string (send ast get-place))) out)]
         
        
        
        [(is-a? ast Op%)
         (display (format "~a@~a"
                        (get-field op ast)
                        (place-to-string (send ast get-place) out)))
         ]


        [(is-a? ast Num%)
	 (if out
	     (display (get-field n (get-field n ast)))
	     (display (format "~a@~a" (get-field n (get-field n ast)) 
			      (place-to-string (send ast get-place) out))))
         ]
      
        [(is-a? ast Array%)
         (display (format "~a[" (get-field name ast)))
	 (send (get-field index ast) accept this)
	 (let ([offset (get-field offset ast)])
	   (when (> offset 0)
		 (display (format "-~a" offset))))
	 (display (format "]"))
         ]
      
        [(is-a? ast Var%)
	 (let ([name (get-field name ast)])
	   ;; (if (equal? name "#return")
	   ;;     (display "return ")
	       (display (format "~a" (get-field name ast))))
         ]
        
        [(is-a? ast UnaExp%)
         (display "(")
         ;(send (get-field op ast) accept this)
	 ;; don't call this because we can have @any @place(i)
	 (display (format "~a@~a " (send (get-field op ast) to-string) 
			  (place-to-string (send ast get-place) out)))
         (send (get-field e1 ast) accept this)
         (display ")")
         ]
        
        [(is-a? ast BinExp%)
         (display "(")
         (send (get-field e1 ast) accept this)
         ;(send (get-field op ast) accept this)
	 ;; don't call this because we can have @any @place(i)
	 (display (format " ~a@~a " (send (get-field op ast) to-string) 
			  (place-to-string (send ast get-place) out)))
         (send (get-field e2 ast) accept this)
         (display ")")
         ]

	[(is-a? ast FuncCall%)
	 (display (format "~a(" (get-field name ast)))
	 (let ([args (get-field args ast)])
	   (unless (empty? args)
		   (send (car args) accept this)
		   (for ([arg (cdr args)])
			(display ", ")
			(send arg accept this))))
	 (display ")")]

	[(is-a? ast Recv%)
	 (display (format "read(~a)" (get-field port ast)))]

	[(is-a? ast Send%)
	 (display (format "send(~a," (get-field port ast)))
	 (send (get-field data ast) accept this)
	 (display ");")]
        
        [(is-a? ast Assign%)
	 (let ([lhs (get-field lhs ast)])
	   (send lhs accept this))
	 (display " = ")
         (send (get-field rhs ast) accept this)
         (display ";")
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
	 (send (get-field pre ast) accept this)
	 (display indent)
	 (display "while(")
	 (send (get-field condition ast) accept this)

         (cond
          [(is-a? ast While==0%) (display " == 0")]
          [(is-a? ast While<0%)  (display " < 0")]
          [(is-a? ast While>=0%) (display " > 0")])

	 (pretty-display ") {")
         (inc-indent)
         (send (get-field body ast) accept this)
	 (send (get-field pre ast) accept this)
         (dec-indent)
         (display (format "~a}" indent))]

        [(is-a? ast For%)
         (pretty-display (format "for(~a from ~a to ~a)@~a {"
			  (send (get-field iter ast) to-string)
			  (get-field from ast)
			  (get-field to ast)
			  (place-to-string (get-field place-list ast))))
	 (inc-indent)
	 (send (get-field body ast) accept this)
	 (dec-indent)
	 (pretty-display (format "~a}" indent))
         ]
        
        [(is-a? ast Block%)
         (for ([stmt (get-field stmts ast)])
	   (unless (is-a? stmt Block%)
                   (newline)
                   (display indent))
           (send stmt accept this)
	   (when (is-a? stmt Exp%)
		 (display ";")))
         (newline)
         ]

        [(is-a? ast FuncDecl%)
         (define (print-arg arg pre)
           (display (format "~a~a@~a ~a" pre
                           (print-type (get-field type arg))
                           (send arg get-place) 
                           (car (get-field var-list arg)))))

         ;; Print function signature
         (let* ([return (get-field return ast)]
		[name (get-field name ast)]
		[type (if return 
                          (get-field type return)
                          "void")])
           (if (pair? type)
               (display (format "~a::~a@~a ~a(" 
				(print-type (car type)) (cdr type)
				(place-to-string (get-field place return))
				name))
	       (if (equal? type "void")
		   (display (format "~a ~a(" (print-type type) name))
		   (display (format "~a@~a ~a(" 
                                    (print-type type) (send return get-place) name)))))

         ;; Print arguments
         (let ([arg-list (get-field stmts (get-field args ast))])
           (when (not (empty? arg-list))
                 (print-arg (car arg-list) "")
                 (for ([arg (cdr arg-list)])
                      (print-arg arg ","))))

         (pretty-display ") {")

         ;; Print Body
         (inc-indent)
         (send (get-field body ast) accept this)
         (dec-indent)
         (pretty-display "}")
         ]
        
        [else (raise "Error: printer unimplemented!")]
        
        ))))
