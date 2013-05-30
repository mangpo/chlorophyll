#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

(define ast-divider%
  (class* object% (visitor<%>)
    (struct core (program workspace stack) #:mutable)

    (super-new)
    (init-field w h [n (* w h)] [cores (make-vector n)])

    ;; Need to set up cores outside the initialization.
    (for ([i (in-range n)])
	 (vector-set! cores i 
		      (let ([block (new Program% [stmts (list)])])
			(core block block (list)))))

   (define (get-program i)
      (core-program (vector-ref cores i)))

    (define (get-workspace i)
      (core-workspace (vector-ref cores i)))

    (define (set-workspace i x)
      (set-core-workspace! (vector-ref cores i) x))

    (define (push-workspace i x)
      (let ([block (core-workspace (vector-ref cores i))])
	(pretty-display `(push-workspace ,i ,x))
        (set-field! stmts block (cons x (get-field stmts block)))
	))

    (define (reverse-workspace i)
      (let ([block (core-workspace (vector-ref cores i))])
        (set-field! stmts block (reverse (get-field stmts block)))))

    (define (get-stack i)
      (begin
	(pretty-display `(get-stack ,i))
	(core-stack (vector-ref cores i))))

    (define (push-stack i x)
      (let ([id (vector-ref cores i)])
	(pretty-display `(push-stack ,i ,(send x to-string)))
        (set-core-stack! id (cons x (core-stack id)))
	))

    (define (pop-stack i)
      (let* ([id (vector-ref cores i)]
             [stack (core-stack id)])
	(pretty-display `(pop-stack ,i -> ,(send (car stack) to-string)))
        (set-core-stack! id (cdr stack))
        (car stack)))

    (define (top-stack i)
      (let* ([id (vector-ref cores i)]
             [stack (core-stack id)])
	(pretty-display `(top-stack ,i -> ,(send (car stack) to-string)))
        (car stack)))

    (define (gen-send x to data)
      (new Send% [data data] [port (direction x to w)]))

    (define (gen-recv x from)
      (new Recv% [port (direction x from w)]))

    (define (transfer from x to)
      (gen-send x to (gen-recv x from)))

    (define (clear-stack c)
      (let ([stack (get-stack c)])
	(pretty-display `(clear-stack ,c))
        (unless (empty? stack)
          (unless (= (length stack) 1)
	    (pretty-display (format "REMAINING ITEMS @CORE ~a:" c))
	    (for ([e stack])
		 (pretty-display (send e to-string)))
            (raise "visitor-divider: thre is more than one EXP remained in the stack!"))
          (push-workspace c (car stack)))))

    (define (reverse-stmts block)
      (set-field! stmts block (reverse (get-field stmts block))))

    (define/public (visit ast)
      (define (gen-comm-path path)    
        (define (intermediate path)
          (if (>= (length path) 3)
              (let ([a (car path)]
                    [b (cadr path)]
                    [c (caddr path)])
                (push-workspace b (transfer a b c)))
              (let ([from (car path)]
                    [to (cadr path)])
                (push-stack to (gen-recv to from)))))
        
        (let ([from (car path)]
              [to (cadr path)])
          (push-workspace from (gen-send from to (top-stack from))))
        (intermediate path))
  
      (define (gen-comm)
        (let ([path (get-field send-path ast)])
          (when path
                ;; (if (list? (car path))
                ;;     (begin
                ;;       (for ([p path])
                ;;            (gen-comm-path p))
                ;;       (pop-stack (caar path)))
                ;;     (begin
                      (gen-comm-path path)
                      (pop-stack (car path)))))

      (define (gen-comm-condition)
	(pretty-display `(gen-comm-condition))
        (let ([path (get-field send-path ast)]
              [place (get-field place-type (get-field condition ast))])
          (define visit (set place))

          (define (gen-condition-path path)
	    (pretty-display `(gen-condition-path ,path))
            (let ([from (car path)]
                  [x (cadr path)])
              (unless (set-member? visit x)
                (set! visit (set-add visit x))
                (push-workspace from (gen-send from x (new Var% [name "#tmp"] [place-type from])))
                (push-workspace x (new Assign% 
                                   ;; special variable
                                   [lhs (new Var% [name "#tmp"] [place-type x])]
                                   [rhs (gen-recv x from)]))
                (push-stack x (new Var% [name "#tmp"] [place-type x]))))
            (when (> (length path) 2)
                    (gen-condition-path (cdr path))))

          
          (when path
		(pretty-display `(gen-comm-condition:push-workspace))
                (push-workspace place (new Assign% 
                                           ;; special variable
                                           [lhs (new Var% [name "#tmp"] [place-type place])]
                                           [rhs (pop-stack place)]))
		(pretty-display `(gen-comm-condition:push-stack))
                (push-stack place (new Var% [name "#tmp"] [place-type place]))
                (for ([p path])
                     (gen-condition-path p)))
        ))

      (define (scope-pattern gen-ast)
        (for ([c (get-field body-placeset ast)])
             (let ([new-ast (gen-ast c)])
               (clear-stack c)     
               (set-field! parent (get-field body new-ast) new-ast)
               (push-workspace c new-ast)
               (set-workspace c (get-field body new-ast))))

        (send (get-field body ast) accept this)
        
        (for ([c (get-field body-placeset ast)])
             (let* ([body (get-workspace c)]
                    [new-ast (get-field parent body)])
               (clear-stack c)
               (reverse-stmts body)
               (set-workspace c (get-field parent new-ast)))))
                

      (cond
       [(is-a? ast Num%)
	(pretty-display (format "\nDIVIDE: Num ~a\n" (send ast to-string)))
        (push-stack (get-field place-type ast) ast)
        (gen-comm)
        ]

       [(is-a? ast Var%)
	(pretty-display (format "\nDIVIDE: Var ~a\n" (send ast to-string)))
        (push-stack (get-field place-type ast) ast)
        (gen-comm)]

       [(is-a? ast Array%)
	(send (get-field index ast) accept this)

	(pretty-display (format "\nDIVIDE: Array ~a\n" (send ast to-string)))
	;; only work for known type for now
	(unless (not (get-field known-type ast))
		(raise "We only handle known-type array for now. Sorry!"))

	(let ([place (get-field place-type ast)])
	  (set-field! index ast (pop-stack place))
	  (push-stack place ast)
	  (gen-comm))]

       [(is-a? ast BinExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (let ([place (get-field place-type ast)])
	  (pretty-display (format "\nDIVIDE: BinExp ~a\n" (send ast to-string)))
          ;; pop in the reverse order
          (set-field! e2 ast (pop-stack place))
          (set-field! e1 ast (pop-stack place))
          (push-stack place ast)
          (gen-comm))]
       
       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)
        (let ([place (get-field place-type ast)])
	  (pretty-display (format "\nDIVIDE: UnaExp ~a\n" (send ast to-string)))
          (set-field! e1 ast (pop-stack place))
          (push-stack place ast)
          (gen-comm))]

       [(is-a? ast FuncCall%)
        (for ([arg (get-field args ast)])
             (send arg accept this))
	(pretty-display (format "\nDIVIDE: FuncCall ~a\n" (send ast to-string)))
        (let ([place (get-field place-type ast)])
          (for ([c (get-field body-placeset (get-field signature ast))])
               (if (not (= place c))
                   (push-workspace c (send ast copy-at c))
                   (send ast copy-ast c)))
          ;; TODO: check when funcCall is stmt not exp!
          (gen-comm))]

       [(is-a? ast ArrayDecl%)
	(let ([place (get-field place ast)])
	  (if (number? place)
	      (push-workspace place ast)
	      (for ([p place])
		   (let ([here (get-field place p)])
		     (push-workspace 
		      here
		      (new ArrayDecl% [var (get-field var ast)]
			   [type (get-field type ast)]
			   [known (get-field known ast)]
			   [bound (- (get-field to p) (get-field from p))]
			   [place-list here]))))))]
       
       [(is-a? ast VarDecl%)
	(let ([place (get-field place ast)])
	  (pretty-display (format "\nDIVIDE: VarDecl ~a@~a\n" (get-field var-list ast) place))
	  (if (number? place)
	      (push-workspace place ast)
	      (for ([p place])
		   (let ([here (get-field place p)])
		     (push-workspace 
		      here
		      (new VarDecl% [var-list (get-field var-list ast)]
			   [type (get-field type ast)]
			   [known (get-field known ast)]
			   [place here]))))))
	  ]

       [(is-a? ast Assign%) 
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)
	(pretty-display (format "\nDIVIDE: Assign\n"))
        (let ([place (get-field place-type (get-field lhs ast))])
          (set-field! rhs ast (pop-stack place))
          (set-field! lhs ast (pop-stack place))
          (push-workspace place ast))]

       [(is-a? ast If%)
	(pretty-display (format "\nDIVIDE: If (condition)\n"))
        (send (get-field condition ast) accept this)
	(pretty-display (format "\nDIVIDE: If (gen-comm-condition)\n"))
        (gen-comm-condition)

	(pretty-display (format "\nDIVIDE: If (true)\n"))
        (for ([c (get-field body-placeset ast)])
             (let* ([old-space (get-workspace c)]
                    ;; pop stack and put in in if condition
                    [new-if (new If% [condition (pop-stack c)]
                                [true-block (new Block% [stmts (list)])]
                                [false-block (new Block% [stmts (list)])]
                                [parent old-space])])
               (clear-stack c)     
               (set-field! parent (get-field true-block new-if) new-if)
               (set-field! parent (get-field false-block new-if) new-if)
               (push-workspace c new-if)
               (set-workspace c (get-field true-block new-if))))

        (send (get-field true-block ast) accept this)

	(pretty-display (format "\nDIVIDE: If (false)\n"))
        (for ([c (get-field body-placeset ast)])
             (let* ([true-block (get-workspace c)]
                    [if (get-field parent true-block)])
               (clear-stack c)
               (reverse-stmts true-block)
               (set-workspace c (get-field false-block if))))

        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
              
        (for ([c (get-field body-placeset ast)])
             (let* ([false-block (get-workspace c)]
                    [if (get-field parent false-block)])
               (clear-stack c)
               (reverse-stmts false-block)
               (set-workspace c (get-field parent if))))]
               
       [(is-a? ast While%)
        (send (get-field condition ast) accept this)
        (gen-comm-condition)
        (scope-pattern 
         (lambda (c) 
           (new While% [condition (pop-stack c)]
                [body (new Block% [stmts (list)])]
                [parent (get-workspace c)])))]

       [(is-a? ast For%)
        (scope-pattern 
         (lambda (c)
           (let ([iter (get-field iter ast)])
             (new For% 
                  [iter (new Var% [name (get-field name (get-field iter ast))])] ;; not clone!
                  [body (new Block% [stmts (list)])]
                  [known #t]
                  [from (get-field from ast)]
                  [to (get-field to ast)]
                  [place-list (get-field place-list ast)]
                  [parent (get-workspace c)]))))]

       [(is-a? ast FuncDecl%)
        (define (func-args-at ast core)
          (new Block% 
               [stmts
                (filter 
                 (lambda (x) (= (get-field place-type x) core))
                 (get-field stmts (get-field args ast)))]))

        (define (func-return-at ast core)
          (let ([return (get-field return ast)])
            (if (and (not (equal? (get-field type return) "void")) 
                     (= (get-field place return) core))
                return
                (new VarDecl% [var-list (list "#return")]
                     [type "void"] [place core] [known (get-field known return)]))))
                       
        (scope-pattern 
         (lambda (c)
           (let ([return (get-field return ast)])
             (new FuncDecl%
                  [name (get-field name ast)]
                  [args (func-args-at ast c)]
                  [return (func-return-at ast c)]
                  [body (new Block% [stmts (list)])]
		  [parent (get-workspace c)]))))]
       

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))

        (when (is-a? ast Program%)
              (for ([i (in-range n)])
                   (unless (is-a? (get-workspace i) Program%) 
                           (raise (format "Top level scope @core ~a is not Program!" i)))
                   (reverse-workspace i))
	      (define programs (make-vector n))
	      (for ([i (in-range n)])
		   (vector-set! programs i (get-workspace i)))
	      programs
	      )
	]

       [else (raise (format "visitor-divider: unimplemented for ~a" ast))]

       )))) 
