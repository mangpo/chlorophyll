#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt")

(provide comm-converter%)

;; Convert partition ID to actual core ID.
;; Note: this visitor mutates AST.
(define comm-converter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field routing-table part2core n)

    (define debug #f)
    
    (define/public (visit ast)
      (define (convert-base p)
        (cond
         [(number? p)
          (vector-ref part2core p)]
         [(list? p)
          (for ([i p])
               (send i accept this))
          p]
         [(pair? p)
          (for ([i (car p)])
               (send i accept this))
          p]
         [(at-global-input? p)
          n]
         [(at-global-output? p)
          (add1 n)]
	 ;; [(is-a? p Place%)
	 ;;  p]
         [(and (is-a? p Place%) (equal? (get-field at p) "any"))
          p]
         [(is-a? p TypeExpansion%)
          (unless (get-field convert p)
                  (set-field! convert p #t)
                  (set-field! place-list p
                              (map (lambda (x) (convert-base x)) (get-field place-list p))))
          p]
         [else
          (raise (format "convert-place-type: unimplemented for ~a" p))]))

      (define (convert-place)
        (set-field! place ast (convert-base (get-field place ast))))

      (define (convert-place-type)
        (set-field! place-type ast (convert-base (get-field place-type ast))))

      (define (convert)
        (unless (get-field convert ast)
                (set-field! convert ast #t)
                (when (field-bound? place ast)
                      (convert-place))
                (when (field-bound? place-type ast)
                      (convert-place-type))))

      (cond
       [(is-a? ast Param%)
        (when debug 
              (pretty-display (format "\nCOMMCONVERT: ~a" (send ast to-string))))
        (convert)
        ]
        
       [(is-a? ast VarDecl%)
        (when debug 
              (pretty-display (format "\nCOMMCONVERT: VarDecl ~a, type:~a" 
				      (get-field var-list ast) (get-field type ast))))
        (unless (equal? (get-field type ast) "void")
          (convert))
        ]
        
       [(is-a? ast Livable%)
        (when debug (pretty-display (format "\nCOMMCONVERT: Livable")))
        (convert)
        ]

       [(is-a? ast LivableGroup%)
        (when debug (pretty-display (format "COMMCONVERT: ArrayDecl")))

        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (number? place)
                    (set-field! place-list ast (vector-ref part2core place)))))
        ]

       [(is-a? ast For%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (number? place)
                    (set-field! place-list ast (vector-ref part2core place)))))
        ]

       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "\nCOMMCONVERT: Num ~a" (send ast to-string))))
        (convert)
        ]

       [(is-a? ast Array%)
        (define index (get-field index ast))
        (send index accept this)
        (convert)
        (when debug 
              (pretty-display (format "\nCOMMCONVERT: Array ~a" (send ast to-string))))
        ]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "COMMCONVERT: Var ~a" (send ast to-string))))
        (convert)
        ]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        ;(define op (get-field op ast))

        (send e1 accept this)
        (send e2 accept this)
        ;(send op accept this)

        (convert)
        (when debug 
              (pretty-display (format "COMMCONVERT: BinExp ~a" (send ast to-string))))
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        ;(define op (get-field op ast))

        (send e1 accept this)
        ;(send op accept this)

	(convert)
        (when debug
              (pretty-display (format "COMMCONVERT: UnaExp ~a" (send ast to-string))))
        ]

       [(is-a? ast FuncCall%)
	(when debug 
	      (pretty-display (format "COMMCONVERT: FuncCall ~a" (get-field name ast))))
	;; recurse on signature
        ;(define args-ret (send (get-field signature ast) accept this))
	(define func-sig (get-field signature ast))
	(define name (get-field name ast))

        (when (is-a? func-sig IOFuncDecl%)
          (send func-sig accept this))

	;; recurse on arguments
	(for ([arg (get-field args ast)])
	     (send arg accept this))

        (when debug 
              (pretty-display (format "COMMCONVERT: FuncCall ~a" (send ast to-string))))
	(convert)
        ]

       [(is-a? ast Assign%) 
        (let ([rhs (get-field rhs ast)]
              [lhs (get-field lhs ast)])
          (when debug 
            (pretty-display (format "COMMCONVERT: Assign ~a = ~a"
                                    (send lhs to-string) (send rhs to-string))))
          (send lhs accept this)
          (send rhs accept this)
          )
        ]

       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            (for ([x val])
                 (send x accept this))
            (send val accept this))
	]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
          (send (get-field false-block ast) accept this))
        (when debug 
              (pretty-display (format "COMMCONVERT: If")))
        ]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        (when debug 
              (pretty-display (format "COMMCONVERT: While")))
        ]

       [(is-a? ast Forever%)
        (send (get-field body ast) accept this)
        ]

       [(is-a? ast Block%) 
        (when debug 
              (pretty-display (format "COMMCONVERT: Block")))
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))
        ]

       [(is-a? ast FuncDecl%)
        (when debug 
	      (pretty-display "\n--------------------------------------------")
              (pretty-display (format "COMMCONVERT: FuncDecl ~a" (get-field name ast))))
        (send (get-field return ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)
        ]
       
       [(is-a? ast ConcreteFilterDecl%)
        (when debug 
	      (pretty-display "\n--------------------------------------------")
              (pretty-display (format "COMMCONVERT: ConcreteFilterDecl ~a" (get-field name ast))))
        (send (get-field input-vardecl ast) accept this)
        (send (get-field output-vardecl ast) accept this)

        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)
        ]

       [else (raise (format "visitor-commconvert: ~a unimplemented" ast))]
       ))
    ))
            
