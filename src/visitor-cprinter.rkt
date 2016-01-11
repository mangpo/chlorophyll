#lang s-exp rosette

(require "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-tempinsert2.rkt")

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
        (+ (* 2 n) port)]))

    (define (print-type type)
      (cond 
       [(equal? type "int") "long"]
       [(equal? type "void") "void"]
       [(fix_t? type) "long"]
       ;;[(fix_t? type) (format "fix~a_t" (fix_t-int type))]
       [(pair? type) (format "~a~a" (print-type (car type)) (cdr type))]
       [else (raise (format "visitor-cprinter: print-type: unimplemented for ~a" type))]))

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
                               (list-to-string (map print-name (get-field var-list ast)) core)
			       ))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a ~a_~a[~a]"
                               (print-type (get-field type ast))
                               (print-name (get-field var ast))
			       core
			       (get-field bound ast)))
         (define init (get-field init ast))
         (when init
               (display " = {")
               (display-list init)
               (display "}"))
         (display ";")
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
	   (cond
            [(> offset 0)
             (display (format "-~a" offset))]
            [(< offset 0)
             (display (format "+~a" (- offset)))]))
	 (display (format "]"))
         ]
      
        [(is-a? ast Var%)
	 (define name (get-field name ast))
	 (define sub (get-field sub ast))

	 ;(unless thread
           ;; this renaming is only relavent for sequential version
           ;(set! sub #f)	
           (when (get-field compact ast)
                 (let ([full-name (regexp-match #rx"(.+)::(.+)" name)])
                   (when full-name
                         ;; "a::0" -> ("a::0" "a" "0")
                         (let* ([actual-name (cadr full-name)]
                                [expand (string->number (caddr full-name))])
                           (set! name actual-name)
                           (set! sub expand)))))
	   ;)

	 (display (format "~a_~a" (print-name name) core))
	 (when sub
	       (display (format ".~a" (vector-ref print-sub sub))))
         ]
        
        [(is-a? ast UnaExp%)
	 (define op (get-field op (get-field op ast)))
         (cond
          [(and (equal? op "**2") (fix_t? (get-field type (get-field e1 ast))))
           (display "finitize(( (abs_tmp=")
           (send (get-field e1 ast) accept this)
           (display (format ") * abs_tmp) >> ~a)" (- 18 (fix_t-int (get-field type (get-field e1 ast))))))
           ]

          [(equal? op "**2")
           (display "finitize( (abs_tmp=")
           (send (get-field e1 ast) accept this)
           (display ") * abs_tmp)")
           ]

          [else
           (display "(")
           (display (send (get-field op ast) to-string))
           (send (get-field e1 ast) accept this)
           (display ")")])
         ]
        
        [(is-a? ast BinExp%)
	 (define op (get-field op (get-field op ast)))
         (cond
          [(and (equal? op "*") (fix_t? (get-field type (get-field e1 ast))))
           (display "finitize((")
           (send (get-field e1 ast) accept this)
           (display op)
           (send (get-field e2 ast) accept this)
           (display 
            (format ") >> ~a)" (- 18 (fix_t-int (get-field type (get-field e1 ast))))))
           ]
           
          [(and (equal? op "/") (fix_t? (get-field type (get-field e1 ast))))
           (display "finitize((")
           (send (get-field e1 ast) accept this)
           (display op)
           (send (get-field e2 ast) accept this)
           (display 
            (format ") << ~a)" (- 18 (fix_t-int (get-field type (get-field e1 ast))))))
           ]

          [(member op (list "/%" "*:2" ">>>"))
           (cond
            [(equal? op "/%") (display "divmod(")]
            [(equal? op "*:2") (display "mult2(")]
            [(equal? op ">>>") (display "rightrot(")]
	    )
           (send (get-field e1 ast) accept this)
           (display ", ")
           (send (get-field e2 ast) accept this)
           (display ")")
           ]
          
          [(member op (list "*/17" "*/16"))
           (display "finitize((")
           (send (get-field e1 ast) accept this)
           (display "*")
           (send (get-field e2 ast) accept this)
           (display ")")
           (if (equal? "op" "*/17")
               (display ">>17)")
               (display ">>16)"))
           ]

          [else
           (display "finitize(")
           (send (get-field e1 ast) accept this)
           (display op)
           (send (get-field e2 ast) accept this)
           (display ")")]
          )]

	[(is-a? ast FuncCall%)
	 (define name (get-field name ast))
	 (define args (get-field args ast))

	 (define fp (or (and (equal? name "out") (fix_t? (get-field type (car args))))
			(and (equal? name "in") (fix_t? (get-field type ast)))))
	 (define fp-ext (if fp "_fp" ""))

         (if (or (equal? name "out") (equal? name "in"))
             (display (format "~a~a(" name fp-ext))
             (display (format "~a_~a(" name core)))

         (unless (empty? args)
                 (send (car args) accept this)
                 (for ([arg (cdr args)])
                      (display ", ")
                      (send arg accept this)))

	 (when fp
	       (if (equal? name "out")
		   (display (format ", ~a" (fix_t-int (get-field type (car args)))))
		   (display (fix_t-int (get-field type ast))))
	       )

         (when (regexp-match #rx"main" name) (display "dummy"))
         (display ")")]
        ;; )]

	[(is-a? ast Recv%)
	 (display (format "read(~a)" (channel core (get-field port ast))))]

	[(is-a? ast Send%)
	 (display (format "write(~a," (channel core (get-field port ast))))
	 (send (get-field data ast) accept this)
	 (display ");")]

        [(is-a? ast PortListen%)
	 (pretty-display
          (format "while(1) { if(read(~a)==999) { act~a_~a(); } else { printf(\"Expect port execution. Receive something else.\"); exit(1); } }"
                  (channel core (get-field port ast))
                  core core))]

        [(is-a? ast PortExec%)
	 (display (format "write(~a,999);" (channel core (get-field port ast))))]
         
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
                 (display (format "long~a(" (length val)))
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

        [(is-a? ast Program%)
         ;; (pretty-display "/*")
         ;; (send ast pretty-print)
         ;; (pretty-display "*/")
         ;; Clone the program AST and replace funccall with temps
         (define new-ast (send ast clone))
         (send new-ast accept (new temp-inserter2% [replace-all #t]))

         (define stmts (get-field stmts new-ast))
         (for ([stmt stmts])
              (when (or (is-a? stmt Block%)
                        (is-a? stmt FuncDecl%)
                        (is-a? stmt VarDecl%)
                        (is-a? stmt ArrayDecl%))
                         
                    (unless (is-a? stmt Block%)
                            (newline)
                            (display indent))
                    (send stmt accept this)
                    ;; (when (is-a? stmt Exp%)
                    ;;       (display ";"))
                    ))
         
         (for ([stmt stmts])
              (when
               (is-a? stmt PortListen%)
               (inc-indent)
               (newline)
               (if thread
                   (begin
                     (pretty-display (format "void *main_~a(void *dummy) {" core))
                     (display indent)
                     (send stmt accept this)
                     (display indent)
                     (pretty-display "return NULL; }")
                     )
                   (begin
                     (pretty-display "int main() {")
                     (display indent)
                     (send stmt accept this)
                     (display indent)
                     (pretty-display "return 0; }")))))
               
         (newline)]
        
        [(is-a? ast Block%)
         (for ([stmt (get-field stmts ast)])
	   (unless (is-a? stmt Block%)
                   (newline)
                   (display indent))
           (send stmt accept this)
	   (when (is-a? stmt Exp%)
		 (display ";")))
         (newline)]

        [(is-a? ast FuncDecl%)
         (define (print-arg arg pre)
           (display (format "~a~a ~a_~a" pre
                           "long"
                           (print-name (car (get-field var-list arg)))
			   core)))

	 (define name (get-field name ast))
         (define return (get-field return ast))
	 (define type (if return 
                          (get-field type return)
                          "void"))
	 (set! expand #f)
         ;; Print function signature
	 (if (equal? name "main")
             ;; main
             (if thread
                 (pretty-display (format "void *main_~a(void *dummy) {" core))
                 (pretty-display "int main() {"))
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
		 (pretty-display (format "~a _return_~a;" (print-type type) core))])

	       (dec-indent)
	       ))

         (inc-indent)

         ;; Declare temps
         (display indent)
         (display (format "int _cond_~a; " core))
         (for ([temp (get-field temps ast)])
              (display (format "long~a ~a_~a; " 
			       (if (> (cdr temp) 1) (cdr temp) "")
			       (car temp) core)))
         (pretty-display "long abs_tmp;")

         ;; Body
         (send (get-field body ast) accept this)
	 (when (equal? name "main")
	       (display indent)
               (if thread
                   (pretty-display "return NULL;")
                   (pretty-display "return 0;"))
               )
         (dec-indent)
         (pretty-display "}")
         ]
        
        [else (raise "Error: printer unimplemented!")]
        
        ))))
