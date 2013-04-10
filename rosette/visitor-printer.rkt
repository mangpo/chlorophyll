#lang s-exp rosette

(require "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define printer%
  (class* object% (visitor<%>)
    (super-new)

    (define indent "")

    (define (inc-indent)
      (set! indent (string-append indent "  ")))

    (define (dec-indent)
      (set! indent (substring indent 2)))
    
    (define/public (visit ast)
      (cond
        [(is-a? ast VarDecl%)
         (display (format "~a@~a ~a;"
                               (get-field type ast)
                               (send ast get-place)
                               (get-field var-list ast)))
         ]
        
        [(is-a? ast ArrayDecl%)
         (display (format "~a@{~a} ~a;"
                               (get-field type ast)
                               (send (get-field place ast) to-string)
                               (get-field var ast)))
         ]
        
        [(is-a? ast Num%)
         (display (format "~a@~a "
                        (get-field n ast)
                        (send ast get-place)))
         ]
        
        [(is-a? ast Op%)
         (display (format "~a@~a "
                        (get-field op ast)
                        (send ast get-place)))
         ]
      
        [(is-a? ast Array%)
         (display (format "~a["
                        (get-field name ast)))
	 (send (get-field index ast) accept this)
	 (display (format "] "))
         ]
      
        [(is-a? ast Var%)
         (display (format "~a "
                        (get-field name ast)))
         ]
        
        [(is-a? ast UnaExp%)
         (send (get-field op ast) accept this)
         (send (get-field e1 ast) accept this)
         ]
        
        [(is-a? ast BinExp%)
         (display "(")
         (send (get-field e1 ast) accept this)
         (send (get-field op ast) accept this)
         (send (get-field e2 ast) accept this)
         (display ")")
         ]
        
        [(is-a? ast Assign%)
         (send (get-field lhs ast) accept this)
         (display "= ")
         (send (get-field rhs ast) accept this)
         ]

        [(is-a? ast For%)
         (pretty-display (format "for(~a from ~a to ~a)@{~a} {"
			  (get-field iter ast)
			  (get-field from ast)
			  (get-field to ast)
			  (send (get-field place ast) to-string)))
	 (inc-indent)
	 (send (get-field block ast) accept this)
	 (dec-indent)
	 (pretty-display (format "~a}" indent))
         ]
        
        [(is-a? ast Block%)
         (for/list ([stmt (get-field stmts ast)])
	   (display indent)
           (send stmt accept this)
           (newline))]
        
        [else (raise "Error: printer unimplemented!")]
        
        ))))
