#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-interface.rkt")

(provide ast-divider%)

(define ast-divider%
  (class* object% (visitor<%>)
    (struct core (program workspace stack temp func) #:mutable)

    (super-new)
    (init-field w h [n (add1 (* w h))] [cores (make-vector n)] [expand-map (make-hash)])

    (define debug #f)

    ;; Need to set up cores outside the initialization.
    (for ([i (in-range n)])
	 (vector-set! cores i 
		      (let ([block (new Program% [stmts (list)])])
			(core block block (list) 0 #f))))

   (define (get-program i)
      (core-program (vector-ref cores i)))

    (define (get-workspace i)
      (core-workspace (vector-ref cores i)))

    (define (set-workspace i x)
      (when debug `(set-workspace ,i ,x))
      (set-core-workspace! (vector-ref cores i) x))

    (define (push-workspace i x)
      (when debug (pretty-display `(push-workspace ,i ,x)))
      (let* ([block (core-workspace (vector-ref cores i))]
	     [stmts (get-field stmts block)]
	     [last-stmt (if (empty? stmts) #f (car stmts))])
	(if (and (is-a? x ArrayDecl%) (is-a? last-stmt ArrayDecl%)
		 (equal? (get-field var x) (get-field var last-stmt)))
	    (set-field! bound last-stmt (+ (get-field bound last-stmt) (get-field bound x)))
	    (set-field! stmts block (cons x (get-field stmts block))))
	))

    (define (reverse-workspace i)
      (let ([block (core-workspace (vector-ref cores i))])
        (set-field! stmts block (reverse (get-field stmts block)))))

    (define (get-stack i)
      (begin
	(when debug (pretty-display `(get-stack ,i)))
	(core-stack (vector-ref cores i))))

    (define (push-stack i x)
      (let ([id (vector-ref cores i)])
	(when debug (pretty-display `(push-stack ,i ,(send x to-string))))
        (set-core-stack! id (cons x (core-stack id)))
	))

    (define (pop-stack i)
      (let* ([id (vector-ref cores i)]
             [stack (core-stack id)])
	(when debug (pretty-display `(pop-stack ,i)))
	(when debug (pretty-display `(pop-stack ,i -> ,(send (car stack) to-string))))
        (set-core-stack! id (cdr stack))
        (car stack)))

    (define (top-stack i)
      (let* ([id (vector-ref cores i)]
             [stack (core-stack id)])
	(when debug (pretty-display `(top-stack ,i -> ,(send (car stack) to-string))))
        (car stack)))

    (define (get-func i)
      (core-func (vector-ref cores i)))

    (define (set-func i func)
      (set-core-func! (vector-ref cores i) func))

    (define (get-temp i)
      (when debug (pretty-display `(get-temp ,i)))
      (let* ([id (vector-ref cores i)]
	     [n (core-temp id)])
	(set-core-temp! id (add1 n))
	(let ([new-temp (format "_tmp~a" n)]
              [func (get-func i)])
          (set-field! temps func (cons new-temp (get-field temps func)))
          new-temp)))

    (define (gen-send x to data)
      (new Send% [data data] [port (direction x to w h)]))

    (define (gen-recv x from)
      (new Recv% [port (direction x from w h)]))

    (define (transfer from x to)
      (gen-send x to (gen-recv x from)))

    (define (clear-stack c)
      (let ([stack (get-stack c)]
	    [count 0])
	(when debug (pretty-display `(clear-stack ,c)))
	(for ([e stack])
	     (cond
	      [(is-a? e FuncCall%)
	       (if (= count 0)
		   (push-workspace c e)
		   (raise (format "@CORE ~a: ~a.\nThere is more than one function call left in the stack!" c (send e to-srting))))
	       (set! count (add1 count))]

	      [(and (is-a? e Var%) (regexp-match #rx"_tmp.*" (get-field name e)))
	       void]))))

    (define (reverse-stmts block)
      (set-field! stmts block (reverse (get-field stmts block))))

    (define/public (visit ast)
      (define (gen-comm-path path)    
        (define (intermediate path)
          (if (>= (length path) 3)
              (let ([a (car path)]
                    [b (cadr path)]
                    [c (caddr path)])
                (push-workspace b (transfer a b c))
		(intermediate (cdr path)))
              (let* ([from (car path)]
		     [to (cadr path)]
		     [temp (get-temp to)])
		(push-workspace to (new Assign%
					[lhs (new Temp% [name temp] [place-type to]
                                                  [type "int"])]
					[rhs (gen-recv to from)]))
                (push-stack to (new Temp% [name temp] [place-type to] [type "int"])))))
        
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
	(when debug (pretty-display `(gen-comm-condition)))
        (let ([path (get-field send-path ast)]
              [place (get-field place-type (get-field condition ast))])
          (define visit (set place))

          (define (gen-condition-path path)
	    ;(pretty-display `(gen-condition-path ,path))
            (let ([from (car path)]
                  [x (cadr path)])
              (unless (set-member? visit x)
                (set! visit (set-add visit x))
                (push-workspace from (gen-send from x (new Temp% [name "_tmp"] [place-type from])))
                (push-workspace x (new Assign% 
                                   ;; special variable
                                   [lhs (new Temp% [name "_tmp"] [place-type x] 
                                             [type "int"])]
                                   [rhs (gen-recv x from)]))
                (push-stack x (new Temp% [name "_tmp"] [place-type x] [type "int"]))))
            (when (> (length path) 2)
                    (gen-condition-path (cdr path))))

          
          (when path
		;(pretty-display `(gen-comm-condition:push-workspace))
                (push-workspace place (new Assign% 
                                           ;; special variable
                                           [lhs (new Temp% [name "_tmp"] 
                                                     [place-type place] 
                                                     [type "int"])]
                                           [rhs (pop-stack place)]))
		;(pretty-display `(gen-comm-condition:push-stack))
                (push-stack place (new Temp% [name "_tmp"] [place-type place] [type "int"]))
                (for ([p path])
                     (gen-condition-path p)))
        ))

      (define (scope-pattern gen-ast)
        ;; create appropriate scope
        (for ([c (get-field body-placeset ast)])
             (let ([new-ast (gen-ast c)])
               (clear-stack c)     
               (set-field! parent (get-field body new-ast) new-ast)
               (push-workspace c new-ast)
               (set-workspace c (get-field body new-ast))))

        ;; visit body
        (send (get-field body ast) accept this)

        ;; update while loop condition
        (when (is-a? ast While%)
              (send (get-field condition ast) accept this)
              (gen-comm-condition))

        ;; set the scope back to the original
        (for ([c (get-field body-placeset ast)])
             (let* ([body (get-workspace c)]
                    [new-ast (get-field parent body)]
		    [old-workspace (get-field parent new-ast)])
               (clear-stack c)
               (reverse-stmts body)
               (set-workspace c old-workspace)

	       ;; remove new-ast if its body is empty
	       (when (and (not (is-a? ast FuncDecl%))
			  (empty? (get-field stmts body)))
		     (set-field! stmts old-workspace (cdr (get-field stmts old-workspace))))
	       )))

      (define (gen-index-vec place-list)
	(define index-vec (make-vector n #f))
	(define count-vec (make-vector n 0))
	(for ([i (in-range (length place-list))]
	      [p place-list])
	     (let ([count (vector-ref count-vec p)])
	       (vector-set! index-vec i count)
	       (vector-set! count-vec p (add1 count))))
	
	(for ([i (in-range (length place-list))]
	      [p place-list])
	     ;; not expand type on that core
	     (when (= (vector-ref count-vec p) 1)
		   (vector-set! index-vec i #f)))

	index-vec)
	
      (cond
       [(is-a? ast Num%)
	(when debug (pretty-display (format "\nDIVIDE: Num ~a\n" (send ast to-string))))
        (push-stack (get-field place-type ast) ast)
        (gen-comm)
        ]

       [(is-a? ast Array%)
	(send (get-field index ast) accept this)

        (when debug
              (pretty-display (format "\nDIVIDE: Array ~a (known=~a)\n" 
                                      (send ast to-string) 
                                      (get-field known-type ast))))
	;; have yet supported clustered array
	(when (get-field cluster ast)
		(raise "We only support non-clustered array for now. Sorry!"))

	(let ([place (get-field place-type ast)])
	  (set-field! index ast (pop-stack place))
	  (push-stack place ast)
	  (gen-comm))]

       [(is-a? ast Var%)
	(when debug (pretty-display (format "\nDIVIDE: Var ~a\n" (send ast to-string))))
        (define place (get-field place-type ast))

	
	(define old-name (get-field name ast))
	;; clean up residual sub
        (set-field! sub ast #f)
	(when (or (regexp-match #rx"_temp" old-name) (regexp-match #rx"#return" old-name))
	      (let ([full-name (regexp-match #rx"(.+)::(.+)" old-name)])
		(when full-name
		      ;; "a::0" -> ("a::0" "a" "0")
		      (let* ([actual-name (cadr full-name)]
			     [expand (string->number (caddr full-name))]
			     [index-vec (dict-ref expand-map actual-name)])
			(set-field! name ast actual-name)
			(set-field! sub ast (vector-ref index-vec expand))))))
	
        (if (number? place)
	    ;; native type
            (push-stack (get-field place-type ast) ast)
            ;; TypeExpansion
            (let ([place-list (get-field place-list place)])
              (for ([p (list->set place-list)])
                (define occur (count (lambda (x) (= x p)) place-list))
                (define type (get-field type ast))

                ;; push
                (push-stack
                 p
                 (new Var% [name (get-field name ast)] [sub (get-field sub ast)]
                      [place-type p]
                      [type (if (= occur 1) type (cons type occur))])))))

        (gen-comm)]

       [(is-a? ast BinExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (let ([place (get-field place-type ast)])
	  (when debug 
                (pretty-display (format "\nDIVIDE: BinExp ~a\n" (send ast to-string))))
          ;; pop in the reverse order
          (set-field! e2 ast (pop-stack place))
          (set-field! e1 ast (pop-stack place))
          (push-stack place ast)
          (gen-comm))]
       
       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)
        (let ([place (get-field place-type ast)])
	  (when debug 
                (pretty-display (format "\nDIVIDE: UnaExp ~a\n" (send ast to-string))))
          (set-field! e1 ast (pop-stack place))
          (push-stack place ast)
          (gen-comm))]

       [(is-a? ast FuncCall%)
	(when debug (pretty-display (format "\nDIVIDE: FuncCall ~a\n" (send ast to-string))))
        (define (func-args-at ast core)
          (filter 
	   (lambda (x) (= (get-field place-type x) core))
	   (get-field stmts (get-field args ast))))

	(define (new-funccall core)
	  (let ([args (func-args-at (get-field signature ast) core)])
	    (new FuncCall% [name (get-field name ast)]
		 ;; reverse order because we pop from stack
		 [args (reverse (map (lambda (x) (pop-stack core)) args))])))

	;; add expressions for arguments
        (for ([arg (get-field args ast)])
             (send arg accept this))

        (define (not-void? place type core)
          (cond
           [(equal? type "void")
            #f]
           [(is-a? place TypeExpansion%)
            (member core (get-field place-list place))]
           [else
            (= place core)]))

	(when debug 
              (pretty-display (format "\nDIVIDE: FuncCall ~a\n" (send ast to-string))))
        (let* ([place (get-field place-type ast)]
	       [sig (get-field signature ast)]
	       [type (get-field type (get-field return sig))])
          (for ([c (get-field body-placeset sig)])
	       ;; body-placeset of IO function is empty
               (if (not-void? place type c)
		   ;; if it is here, funccall is exp
                   (push-stack c (new-funccall c))
		   ;; if return place is not here, funcall is statement
                   (push-workspace c (new-funccall c))))
          (gen-comm))]

       [(is-a? ast ArrayDecl%)
	(when debug 
	      (pretty-display (format "\nDIVIDE: ArrayDecl\n")))

	(let ([place (get-field place-list ast)])
	  (if (number? place)
	      (push-workspace place ast)
	      (for ([p place])
		   (let ([here (get-field place p)])
		     (pretty-display `(array ,(get-field var ast) ,here 
					     ,(get-field from p)
					     ,(get-field to p)))
		     (push-workspace 
		      here
		      (new ArrayDecl% [var (get-field var ast)]
			   [type (get-field type ast)]
			   [known (get-field known ast)]
			   [bound (- (get-field to p) (get-field from p))]
			   [cluster (get-field cluster ast)]
			   [place-list here]))))))]
       
       [(is-a? ast VarDecl%)
	(define place (get-field place ast))
	(when debug 
	      (pretty-display (format "\nDIVIDE: VarDecl ~a@~a\n" 
				      (get-field var-list ast) place))
	      )
	(cond
	 [(number? place)
	  (push-workspace place ast)]

	 [(list? place)
	  (for ([p place])
		   (let ([here (get-field place p)])
		     (push-workspace 
		      here
		      (new VarDecl% [var-list (get-field var-list ast)]
			   [type (get-field type ast)]
			   [known (get-field known ast)]
			   [place here]))))]

	 [(is-a? place TypeExpansion%)
	  (define place-list (get-field place-list place))
	  (for ([p (list->set place-list)])
               (define occur (count (lambda (x) (= x p)) place-list))
               (define type (car (get-field type ast)))
               (push-workspace
                p
                (new VarDecl% [var-list (get-field var-list ast)]
                     [type (if (= occur 1) type (cons type occur))]
                     [known (get-field known ast)]
                     [place p]))
	       )

	  ;; update expand-map
	  (let ([index-vec (gen-index-vec place-list)])
	    (for ([name (get-field var-list ast)])
		 (dict-set! expand-map name index-vec)))
	  ])]

       [(is-a? ast Assign%) 
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)
	(when debug (pretty-display (format "\nDIVIDE: Assign\n")))
        (let ([place (get-field place-type (get-field lhs ast))])
          (if (number? place)
              (begin
                (set-field! rhs ast (pop-stack place))
                (set-field! lhs ast (pop-stack place))
                (push-workspace place ast))
              (for ([p (list->set (get-field place-list place))])
                   (push-workspace p (new Assign% 
                                          ;; pop rhs before lhs!
                                          [rhs (pop-stack p)] 
                                          [lhs (pop-stack p)])))))]

       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            ;; list
            (let ([placeset (list->set (map (lambda (x) (get-field place-type x)) val))])
              (for ([p val])
                   (send p accept this))
              (for ([p placeset])
                   (let* ([occur (count (lambda (x) (= p (get-field place-type x))) val)]
                          [p-val (reverse (for/list ([i (in-range occur)])
                                                    (pop-stack p)))])
                     (push-workspace 
                      p 
                      (new Return% [val (if (= 1 (length p-val)) (car p-val) p-val)])))))
            ;; exp
            (let ([place (get-field place-type val)])
              (send val accept this)
              (push-workspace place (pop-stack place))))]

       [(is-a? ast If%)
	(when debug (pretty-display (format "\nDIVIDE: If (condition)\n")))
        (send (get-field condition ast) accept this)
	(when debug (pretty-display (format "\nDIVIDE: If (gen-comm-condition)\n")))
        (gen-comm-condition)

	(when debug (pretty-display (format "\nDIVIDE: If (true)\n")))
	;; add If AST and prepare for true-block
        (for ([c (get-field body-placeset ast)])
             (let* ([old-space (get-workspace c)]
                    ;; pop stack and put in in if condition
		    [new-if (get-new-if ast
					(pop-stack c) ;; condition
					(new Block% [stmts (list)]) ;; t
					(new Block% [stmts (list)]) ;; f
					#f ;; body-placeset
					old-space)]) ;; parent
               (clear-stack c)     
               (set-field! parent (get-field true-block new-if) new-if)
               (set-field! parent (get-field false-block new-if) new-if)
               (push-workspace c new-if)
               (set-workspace c (get-field true-block new-if))))

	;; add content inside true-block
        (send (get-field true-block ast) accept this)

	;; prepare for false-block
	(when debug (pretty-display (format "\nDIVIDE: If (false)\n")))
        (for ([c (get-field body-placeset ast)])
             (let* ([true-block (get-workspace c)]
                    [if (get-field parent true-block)])
               (clear-stack c)
               (reverse-stmts true-block)
               (set-workspace c (get-field false-block if))))

	;; add content inside false-block
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
              
	;; pop scope
        (for ([c (get-field body-placeset ast)])
             (let* ([false-block (get-workspace c)]
                    [if (get-field parent false-block)]
		    [old-workspace (get-field parent if)])
               (clear-stack c)
               (reverse-stmts false-block)
               (set-workspace c old-workspace)

	       ;; remove new-ast if its true-block and false-block are empty
	       (when (and (empty? (get-field stmts (get-field true-block if)))
			  (empty? (get-field stmts false-block)))
		     (set-field! stmts old-workspace (cdr (get-field stmts old-workspace))))))
	]
               
       [(is-a? ast While%)
	(when debug (pretty-display (format "\nDIVIDE: While\n")))
        (send (get-field condition ast) accept this)
        (gen-comm-condition)
        (scope-pattern 
         (lambda (c) 
	   (get-new-while ast
			  (pop-stack c) ;; condition
			  (new Block% [stmts (list)]) ;; body
			  #f #f ;; bound, body-placeset
			  (get-workspace c))))] ;; parent

       [(is-a? ast For%)
	(when debug (pretty-display (format "\nDIVIDE: For\n")))
        (scope-pattern 
         (lambda (c)
           (let ([iter (get-field iter ast)])
             (new For% 
                  [iter (new Var% [name (get-field name (get-field iter ast))]
                             [type "int"])] ;; not clone!
                  [body (new Block% [stmts (list)])]
                  [known #t]
                  [from (get-field from ast)]
                  [to (get-field to ast)]
                  [place-list (get-field place-list ast)]
                  [parent (get-workspace c)]))))]

       [(is-a? ast FuncDecl%)
	(when debug (pretty-display (format "\nDIVIDE: FuncDecl ~a\n" (get-field name ast))))
        (define (func-args-at ast core)
          (new Block% 
               [stmts
                (filter 
                 (lambda (x) (= (get-field place-type x) core))
                 (get-field stmts (get-field args ast)))]))

        (define (func-return-at ast core)
          (let ([return (get-field return ast)])
            (cond
             ;; [(list? return)
             ;;  (let ([l (length (filter (lambda (x) (= (get-field place x) core)) return))])
             ;;    (new VarDecl% 
             ;;         [var-list (list "#return")]
             ;;         [type (if (= l 0)
             ;;                   "void"
             ;;                   (cons (get-field type (car return)) l))]
             ;;         [place core]
             ;;         [known (get-field known (car return))]))
             ;;  ]

             [(is-a? (get-field place return) TypeExpansion%)
              (define place-list (get-field place-list (get-field place return)))

	      ;; update expand-map
	      (dict-set! expand-map "#return" (gen-index-vec place-list))

              (define occur (count (lambda (x) (= x core)) place-list))
              (define type (car (get-field type return)))
              (if (> occur 0)
                  (new VarDecl% [var-list (list "#return")]
                       [type (if (= occur 1) type (cons type occur))]
                       [place core])
                  (new VarDecl% 
                   [var-list (list "#return")]
                   [type "void"] [place core] [known (get-field known return)]))
              ]

             [(and (not (equal? (get-field type return) "void")) 
                     (= (get-field place return) core))
              return]

             [else
              (new VarDecl% 
                   [var-list (list "#return")]
                   [type "void"] [place core] [known (get-field known return)])])))
                       
        (scope-pattern 
         (lambda (c)
           (let ([return (get-field return ast)]
                 [func (new FuncDecl%
                            [name (get-field name ast)]
                            [args (func-args-at ast c)]
                            [return (func-return-at ast c)]
                            [body (new Block% [stmts (list)])]
                            [parent (get-workspace c)])])
             (set-func c func)
             func)))]
         
       [(is-a? ast ConcreteFilterDecl%)
	(when debug (pretty-display (format "\nDIVIDE: ConcreteFilterDecl ~a\n" (get-field name ast))))
        (scope-pattern 
         (lambda (c)
           (let ([func (new FuncDecl%
                            [name "main"]
                            [return (new ReturnDecl%
                                         [var-list (list "#return")]
                                         [type "void"]
                                         [place #f])]
                            [args (new Block% [stmts (list)])]
                            [body (new Block% [stmts (list)])]
                            [parent (get-workspace c)])])
             (set-func c func)
             func)))]

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (unless (is-a? stmt IOFuncDecl%)
               (send stmt accept this)))

        (when (is-a? ast Program%)
              (for ([i (in-range n)])
                   (unless (is-a? (get-workspace i) Program%) 
                           (raise (format "Top level scope @core ~a is not Program!" i)))
                   (reverse-workspace i))
	      (define programs (make-vector n))
	      (for ([i (in-range n)])
		   (vector-set! programs i (get-workspace i)))

	      (for ([i (in-range n)])
		   (let* ([program (vector-ref programs i)]
			  [stmts (get-field stmts program)])
		     (unless (or (empty? stmts)
				 (equal? (get-field name (last stmts)) "main"))
			   (set-field! stmts program (list)))))
				     
	      programs
	      )
	]

       [else (raise (format "visitor-divider: unimplemented for ~a" ast))]

       ))))
