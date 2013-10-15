#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define offset-modifier%
  (class* object% (visitor<%>)
    (super-new)
    ;; Collect iter offset to adjust for loop bound (offset).
    ;; This is not for address field.
    (init-field [iter-map (make-hash)] [offset-map (make-hash)] [prohibit (set)] 
                [level 0])
    (define debug #t)

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" iter-map)
        (set! iter-map new-env))
      )

    (define (pop-scope)
      (set! iter-map (dict-ref iter-map "__up__")))

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (set)]

       [(is-a? ast Array%)
        (when debug (pretty-display (format "OFFSET: Array ~a offset = ~a" (get-field name ast) (hash-ref offset-map (get-field name ast)))))
        (set-field! offset ast (hash-ref offset-map (get-field name ast)))

        (define save-level level)
        (set! level (add1 level))
	(define index (get-field index ast))
	(define index-ret (send index accept this))
        (set! level save-level)

        ;; (define offset (get-field offset ast))
        ;; (when (and (not (get-field simple-expr ast)) 
        ;;            (> offset 0))
        ;;   ;; if index is not simple and offset > 0
        ;;   (set-field! index ast (new BinExp% [op (new Op% [op "-"])]
        ;;                              [e1 index] 
        ;;                              [e2 (new Num% [n (new Const% [n offset])])]))
        ;;   (set-field! offset ast 0))

        (if (= level 0)
            (for ([x index-ret])
                 (update-name iter-map x (cons ast (lookup-name iter-map x))))
            (begin
              (when debug (pretty-display `(array-prohibit ,index-ret)))
              (set! prohibit (set-union prohibit index-ret))))
        (set)
        ]

       [(is-a? ast Temp%)
        (define link (get-field eqv ast))
        (when debug
              (pretty-display (format "OFFSET: Temp ~a, link = ~a" (send ast to-string)
                                      link)))

        (define ret
          (if link
              (get-field info link)
              (set)))
        (when debug (format "ret = ~a" ret))
        ret
          ]

       [(is-a? ast Var%)
        (define name (get-field name ast))
        (when debug
              (pretty-display (format "OFFSET: Var ~a, has-var? ~a"
                                      name (has-var? iter-map name))))
        (if (has-var? iter-map name)
            (set name)
            (set))
        ]
        
       [(is-a? ast UnaExp%)
	(define e1-ret (send (get-field e1 ast) accept this))
        (define op-str (get-field op (get-field op ast)))
        (cond 
         [(equal? op-str "+")
          e1-ret]
         [else
          (when debug (pretty-display `(unaexp-prohibit ,e1-ret)))
          (set! prohibit (set-union prohibit e1-ret))
          (set)])
          ]

       [(is-a? ast BinExp%)
	(define e1-ret (send (get-field e1 ast) accept this))
	(define e2-ret (send (get-field e2 ast) accept this))

        (define op-str (get-field op (get-field op ast)))
        (define iter-vars (set-union e1-ret e2-ret))
        (cond
         [(equal? op-str "+")
          iter-vars]
         [(equal? op-str "-")
          (when debug (pretty-display `(binexp-prohibit- ,e2-ret)))
          (set! prohibit (set-union prohibit e2-ret))
          e1-ret]
         [else
          (when debug (pretty-display `(binexp-prohibit* ,iter-vars)))
          (set! prohibit (set-union prohibit iter-vars))
          (set)])
	]
        
       [(is-a? ast FuncCall%)
        (define iter-vars (list))
	(for ([arg (get-field args ast)])
	     (set! iter-vars (append iter-vars 
                                     (send arg accept this))))

        
        (when (and debug (not (set-empty? iter-vars)))
              (pretty-display `(funccall-prohibit ,iter-vars)))
        (set! prohibit (set-union prohibit iter-vars))
        (set)
        ]

       [(is-a? ast Recv%)
        (set)]

       [(is-a? ast Send%)
	(define data-ret (send (get-field data ast) accept this))
        (when debug (pretty-display (format "OFFSET: Send ~a, ret = ~a, prohibit = ~a" 
                                            (send (get-field data ast) to-string)
                                            data-ret prohibit)))
        (when (and debug (not (set-empty? data-ret)))
              (pretty-display `(funccall-prohibit ,data-ret)))
        (set! prohibit (set-union prohibit data-ret))
        ]
        
       [(is-a? ast Assign%)
	(send (get-field lhs ast) accept this)
	(define rhs-ret (send (get-field rhs ast) accept this))
        (when (is-a? ast AssignTemp%)
              (set-field! info ast rhs-ret))
        ]

       [(is-a? ast ArrayDecl%)
        (when debug (pretty-display (format "OFFSET: ArrayDecl ~a offset = ~a" (get-field var ast) (get-field offset ast))))
        (hash-set! offset-map (get-field var ast) (get-field offset ast))]

       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            (for ([v val])
                 (send v accept this))
            (send (get-field val ast) accept this))
	]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
	(push-scope)
	(send (get-field true-block ast) accept this)
	(pop-scope)
	(when (get-field false-block ast)
	      (push-scope)
	      (send (get-field false-block ast) accept this)
	      (pop-scope))]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
	(push-scope)
	(send (get-field body ast) accept this)
	(pop-scope)
	]

       [(is-a? ast For%)
	(push-scope)

	(define iter-name (get-field name (get-field iter ast)))
        (define iter-type (get-field iter-type ast))
        (declare iter-map iter-name (list))
	(send (get-field body ast) accept this)

	(define arrays (lookup-name iter-map iter-name))
        (when debug
              (pretty-display (format "OFFSET: For ~a from ~a to ~a" iter-name
                                      (get-field from ast) (get-field to ast)))
              (pretty-display ">> arrays:")
              (for ([array arrays])
                   (display (format "~a, " (send array to-string))))
              (newline)
              (pretty-display (format ">> prohibit: ~a" prohibit))
              )
	(unless (or (empty? arrays)
                    (set-member? prohibit iter-name))
		;; (define min-offset (foldl (lambda (x min-so-far) 
                ;;                             (min (get-field offset x) min-so-far))
		;; 			  (get-field from ast) arrays))
                (define min-offset (get-field from ast))
                (when debug
                      (pretty-display `(min-offset ,min-offset)))
		(when (> min-offset 0)
		      (for ([array arrays])
			   (set-field! offset array (- (get-field offset array) min-offset)))
		      (set-field! from ast (- (get-field from ast) min-offset))
		      (set-field! to ast (- (get-field to ast) min-offset))))
        (set! prohibit (set-remove prohibit iter-name))
	     
	(pop-scope)
	]

       [(is-a? ast FuncDecl%)
	(push-scope)
	(for ([arg (reverse (get-field stmts (get-field args ast)))])
	     (send arg accept this))
	(send (get-field body ast) accept this)
	(pop-scope)
	]
	
       [(is-a? ast Program%)
        (when debug
              (pretty-display "--------------------------------------------")
              (send ast pretty-print))
	(for ([decl (get-field stmts ast)])
	     (send decl accept this))]

       [(is-a? ast Block%)
	(for ([stmt (get-field stmts ast)])
	     (send stmt accept this))]

       [else 
	(raise (format "visitor-offset: unimplemented for ~a" ast))]
       ))))
       