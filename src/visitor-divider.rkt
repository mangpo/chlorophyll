#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt"
         "visitor-offset.rkt"
         "visitor-interface.rkt")

(provide ast-divider%)

;; 1) Separate program IR into multiple IRs for multiple cores.
;; 2) Insert communication code at appropriate places.
;; 3) Insert "ghost" temporary variables whose names start with _dummy
;;    to enforce the same order of execution of expressions.
;;    EXCEPTION: no "ghost" temp for 
;;               - lhs variable
;;               - index of array when it is Var%
(define ast-divider%
  (class* object% (visitor<%>)
    (struct core (program workspace stack temp func) #:mutable)

    (super-new)
    (init-field routing-table actors actors*-no-cf-map
                w h [n (add1 (* w h))]
                [cores (make-vector n)] [expand-map (make-hash)])

    (define debug #f)
    (define visited-actor-funcs (list))
    (define visited-actor-pairs (list))

    ;; When is-lhs is true, no ghost temp for Var% and Array%
    (define is-lhs #f)
    ;; When is-index-var is true, no ghost temp for Var%
    (define is-index #f)
    (define is-io-arg #f)

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
      (when debug (pretty-display `(set-workspace ,i ,x)))
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

    (define (clear-workspace i)
      (when debug (pretty-display `(clear-workspace ,i)))
      (define (clear x)
        (define parent (get-field parent x))
        (if parent
            (begin
              (when (is-a? x Block%) (reverse-stmts x))
              (clear parent))
            x))

      (define top (clear (get-workspace i)))
      ;; (pretty-display `(clear-workspace ,i ,top))
      (set-workspace i top))
    
    (define (set-workspace-actor i from)
      (define ws (get-workspace i))
      (when debug (pretty-display `(set-workspace-actor ,i ,ws)))
      (push-workspace i (new PortListen% [port (direction i from w h)]))

      (define (top-level x)
        (define parent (get-field parent x))
        (if parent
            (begin
              (when (is-a? x Block%) (reverse-stmts x))
              (top-level parent)
              )
            x))

      ;; create handler function.
      (define top (top-level ws))
      (define body (new BlockActor% [stmts (list)]))
      (define new-func
        (new FuncDecl% [name (format "act~a" i)]
             [args (new Block% [stmts (list)])]
             [return #f]
             [body body]
             [parent top]))
      (set-field! parent body new-func)
      (set-field! stmts top (cons new-func (get-field stmts top)))
      (set-workspace i body)
      (set-func i new-func)
      )

    (define (add-while i)
      (define ws (get-workspace i))
      (define need-cleanup #f)
      (when
       (is-a? ws Program%)
       ;; only do this if that node don't have other computation.
       (set! need-cleanup #t)
       (define main-body (new Block% [stmts (list)]))
       (define main-func
         (new FuncDecl% [name "main"]
              [args (new Block% [stmts (list)])]
              [return #f]
              [body main-body]
              [parent ws]))
       (set-field! parent main-body main-func)
       (push-workspace i main-func)
       (set-workspace i main-body)
       (set-func i main-func)
       
       ;; create while(1)
       (define body (new BlockActor% [stmts (list)]))
       (define new-while
         (new While%
              [condition (new Num% [n (new Const% [n 1])] [type "int"])]
              [body body]
              [bound 100] [parent (get-workspace i)]))
       (set-field! parent body new-while)
       (push-workspace i new-while)
       (set-workspace i body))
      need-cleanup
      )

    (define (add-remote-exec i to node)
      (define new-exec (new PortExec% [name (format "act~a" node)]
                            [port (direction i to w h)] [node node]))
      (push-workspace i new-exec)
      )

    (define (get-stack i)
      (begin
	(when debug (pretty-display `(get-stack ,i)))
	(core-stack (vector-ref cores i))))

    (define (push-stack i x)
      (let ([id (vector-ref cores i)])
	(when debug (pretty-display `(push-stack ,i ,(send x to-string) ,(core-stack id))))
        (set-core-stack! id (cons x (core-stack id)))
	))

    (define (push-stack-temp c x)
      (let* ([place-type (get-field place-type x)]
	     [temp (get-temp c (if (list? place-type) (length place-type) 1))]
             [stmt (new AssignTemp% 
                        [lhs (new Temp% [name temp] [place-type place-type] 
                                  [type (get-field type x)])]
                        [rhs x])])
	(push-workspace c stmt)
	(if (and (list? place-type) (> (length place-type) 1))
	    (for ([i (in-range (length place-type))])
		 (push-stack c (new Temp% [name temp] [place-type c] 
				    [sub i] [compact #t]
				    [type (get-field type x)])))
            (push-stack c (new Temp% [name temp] [place-type c]
                               [type (get-field type x)] [eqv stmt])))))

    (define (pop-stack i)
      (let* ([id (vector-ref cores i)]
             [stack (core-stack id)])
	(when debug 
          (pretty-display `(pop-stack ,i))
          (pretty-display `(top    -> ,(send (car stack) to-string)))
          (pretty-display `(remain -> ,(cdr stack))))
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

    (define (get-temp i entry)
      (when debug (pretty-display `(get-temp ,i)))
      (let* ([id (vector-ref cores i)]
	     [n (core-temp id)])
	(set-core-temp! id (add1 n))
	(let ([new-temp (format "_dummy~a" n)]
              [func (get-func i)])
          (set-field! temps func (cons (cons new-temp entry) (get-field temps func)))
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
		   (raise (format "@CORE ~a: ~a.\nThere is more than one function call left in the stack!" c (send e to-string))))
	       (set! count (add1 count))]

	      [(and (is-a? e Var%) (regexp-match #rx"_cond.*" (get-field name e)))
	       (if (= count 0)
                   (set! count (add1 count))
                   (raise (format "@CORE ~a.\nThere is more than one _cond left in the stack!" c)))
               ]

              [else
               (raise (format "@CORE ~a: ~a.\nThere is something left in the stack!" 
                              c (send e to-string)))]
              ))
        (set-core-stack! (vector-ref cores c) (list))))

    (define (reverse-stmts block)
      (set-field! stmts block (reverse (get-field stmts block))))

    (define (reverse-workspace i)
      (let ([block (core-workspace (vector-ref cores i))])
        (set-field! stmts block (reverse (get-field stmts block)))))


    (define (topo-sort pairs)
      (define mapping (make-hash pairs))
      (define actors (map car pairs))
      (define order (list))
      
      (define (dfs actor)
        ;; drop actor
        (define caller (hash-ref mapping actor))
        (define path (drop (vector-2d-ref routing-table actor caller) 1))
        (for ([i path])
             (when (member i actors) (dfs i)))
        (set! order (cons (cons actor caller) order))
        (set! actors (remove actor actors)))

      (define (loop) (unless (empty? actors) (dfs (car actors)) (loop)))
      (loop)
      (reverse order))

    (define/public (visit ast)
      (define (gen-comm-path path type)    
        (define (intermediate path)
          (if (>= (length path) 3)
              (let ([a (car path)]
                    [b (cadr path)]
                    [c (caddr path)])
                (push-workspace b (transfer a b c))
		(intermediate (cdr path)))
              (let* ([from (car path)]
		     [to (cadr path)]
		     [temp (get-temp to 1)]
                     [stmt (new AssignTemp%
                                [lhs (new Temp% [name temp] [place-type to]
                                          [type type])]
                                [rhs (gen-recv to from)])])
                ;; Need to introduce them here. 
                ;; Consider: sum1(a@1, sum2(a@1, b@0))
                ;; at core 2
                ;; sum2(read(1)); sum1(read(1));
                ;; notice that the order of reading from 1 is swaped.
		(push-workspace to stmt)
                (push-stack to (new Temp% [name temp] [place-type to] [type type]
                                    [eqv stmt])))))
        
        (let ([from (car path)]
              [to (cadr path)])
          (push-workspace from (gen-send from to (top-stack from))))
        (intermediate path))
  
      (define (gen-comm)
        ;; When place-type of function call is a list of ProxyReturn, send-path is false.
        ;; In such case, we gauruntee that there is no communication.
        ;; Scenario 1: tuple places of function call return match with params'
        ;;             => no comm.
        ;; Scenario 2: tuple places of function call return don't match
        ;;             => temp = func()
        ;;     
        ;; Therefore, we don't have to worry about such case in gen-comm.
        (let ([path (get-field send-path ast)])
          (when path
                (gen-comm-path path (get-field type ast))
                (pop-stack (car path)))))

      (define (gen-comm-condition)
        (define body (get-field body-placeset ast))
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
                (push-workspace from (gen-send from x (new Temp% [name "_cond"] [place-type from])))
                (push-workspace x (new Assign% 
                                   ;; special variable
                                   [lhs (new Temp% [name "_cond"] [place-type x] 
                                             [type "int"])]
                                   [rhs (gen-recv x from)]))
                (when (set-member? body x)
                      (push-stack x (new Temp% [name "_cond"] [place-type x] [type "int"])))))
            (when (> (length path) 2)
                    (gen-condition-path (cdr path))))

          
          (when path
		;;(pretty-display `(gen-comm-condition:push-workspace))
                (push-workspace place (new Assign% 
                                           ;; special variable
                                           [lhs (new Temp% [name "_cond"] 
                                                     [place-type place] 
                                                     [type "int"])]
                                           [rhs (pop-stack place)]))
                ;;(pretty-display `(gen-comm-condition:push-stack))
                
                (when (set-member? body place)
                      (push-stack place (new Temp% [name "_cond"] [place-type place] [type "int"])))
                (for ([p path])
                     (gen-condition-path p)))
        ))

      (define (scope-pattern gen-ast)
	(define body-placeset (get-field body-placeset ast))
        ;; create appropriate scope
        (for ([c (get-field body-placeset ast)])
             (let ([new-ast (gen-ast c)])
               (clear-stack c)     
               (set-field! parent (get-field body new-ast) new-ast)
               (push-workspace c new-ast)
               (set-workspace c (get-field body new-ast))))

	(when (is-a? ast While%)
	  ;; switch to precondition scope
	  (for ([c (get-field body-placeset ast)])
	       (let* ([body (get-workspace c)]
		      [while (get-field parent body)]
		      [pre (get-field pre while)])
		 (set-field! parent pre while)
		 (set-workspace c pre)))

          (if (is-a? (get-field condition ast) Num%)
              (for ([c (get-field body-placeset ast)])
                (push-stack c (get-field condition ast)))
              (begin
                (send (get-field pre ast) accept this)
                (send (get-field condition ast) accept this)
                (gen-comm-condition)))

	  ;; switch back to body scope
	  (for ([c (get-field body-placeset ast)])
	       (let* ([pre (get-workspace c)]
		      [while (get-field parent pre)])
		 (set-field! condition while (pop-stack c))
		 (reverse-stmts pre)
		 (clear-stack c)     
		 (set-workspace c (get-field body while)))))

        ;; visit body
        (send (get-field body ast) accept this)

        ;; ;; update while loop condition
        ;; (when (is-a? ast While%)
        ;;       (send (get-field condition ast) accept this)
        ;;       (gen-comm-condition))

        ;; set the scope back to the original
        (for ([c (get-field body-placeset ast)])
             (let ([body (get-workspace c)])
               (unless
                (is-a? body Program%)
                ;; c can be Program% if it's an actor.
                (let* ([new-ast (get-field parent body)]
                       [old-workspace (get-field parent new-ast)])
                  (clear-stack c)
                  (reverse-stmts body)
                  (set-workspace c old-workspace)

                  ;; remove new-ast if its body is empty
                  (when (and (not (is-a? ast FuncDecl%))
                             (empty? (get-field stmts body)))
                        (set-field! stmts old-workspace (cdr (get-field stmts old-workspace))))
                  )))))

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
        (if is-io-arg
            (push-stack (get-field place-type ast) ast)
            (push-stack-temp (get-field place-type ast) ast))
        (gen-comm)
        ]

       [(is-a? ast Array%)
	;; have yet supported clustered array
	(when (get-field cluster ast)
		(raise "We only support non-clustered array for now. Sorry!"))

	(define place (get-field place-type ast))
	(unless (unique-place ast)
	  (define my-lhs is-lhs)
	  (set! is-lhs #f)
	  (set! is-index #t)
	  (send (get-field index ast) accept this)
	  (set! is-index #f)
	  (set! is-lhs my-lhs)
	  (set-field! index ast (pop-stack place))
	  )

        (when debug
              (pretty-display (format "\nDIVIDE: Array ~a (place@~a) (index@~a) (unique@~a)\n" 
                                      (send ast to-string) 
                                      (get-field place-type ast) 
				      (get-field place-type (get-field index ast))
				      (unique-place ast)))
	      )
				      

        (unless is-lhs
		(push-stack-temp place ast))
	(gen-comm)]

       [(is-a? ast Var%)
        (define place (get-field place-type ast))
	(when debug (pretty-display (format "\nDIVIDE: Var ~a @~a\n" (send ast to-string)
                                            place)))

	
	(define old-name (get-field name ast))
	;; clean up residual sub
        ;; (set-field! sub ast #f)
	(when (get-field compact ast)
              (let ([full-name (regexp-match #rx"(.+)::(.+)" old-name)])
                (when full-name
                      ;; "a::0" -> ("a::0" "a" "0")
                      (let ([actual-name (cadr full-name)]
                            [expand (string->number (caddr full-name))])
                        (set-field! name ast actual-name)
                        (set-field! sub ast expand))))
              
              (when (get-field sub ast)
                    (let* ([name (get-field name ast)]
                           [index-vec (dict-ref expand-map name)])
                      (set-field! sub ast (vector-ref index-vec (get-field sub ast))))))

        (when debug
              (pretty-display (format "NAME: ~a ~a" (get-field name ast) (get-field sub ast))))
	
	(unless is-lhs
		(if (number? place)
		    ;; native type
		    ((if is-index push-stack push-stack-temp)
		     (get-field place-type ast) ast)
		    ;; TypeExpansion
		    (let ([place-list (get-field place-list place)])
		      (for ([p (list->set place-list)])
			   (define occur (count (lambda (x) (= x p)) place-list))
			   (define type (get-field type ast))
			   
			   ;; push
			   ((if is-index push-stack push-stack-temp)
			    p
			    (new Var% [name (get-field name ast)] [sub (get-field sub ast)]
				 [place-type p]
				 [type (if (= occur 1) type (cons type occur))]))))))

        (gen-comm)]

       [(is-a? ast BinExp%)
        (define place (get-field place-type ast))
	(set! is-index #f)
	
	(unless (unique-place ast)
		(send (get-field e1 ast) accept this)
		(send (get-field e2 ast) accept this)
		;; pop in the reverse order
		(set-field! e2 ast (pop-stack place))
		(set-field! e1 ast (pop-stack place)))

        (when (member (get-field op (get-field op ast)) (list "/%" "*:2" ">>:2" ">>>"))
              (let ([r (new ProxyReturn% [place-type place])])
                (set-field! place-type ast (list r r))))
	
	(when debug 
	      (pretty-display (format "\nDIVIDE: BinExp ~a\n" (send ast to-string))))

        (push-stack-temp place ast)
	(gen-comm)
        ]
       
       [(is-a? ast UnaExp%)
	(define place (get-field place-type ast))
	(set! is-index #f)
	
	(unless (unique-place ast)
		(send (get-field e1 ast) accept this)
		(set-field! e1 ast (pop-stack place)))
	
	(when debug 
	      (pretty-display (format "\nDIVIDE: UnaExp ~a\n" (send ast to-string))))
	(push-stack-temp place ast)
	(gen-comm)]

       [(is-a? ast FuncCall%)
	(set! is-index #f)
        (define name (get-field name ast))
        (define return-place (get-field place-type ast))
        (define type (get-field type ast))
	(when debug
              (pretty-display (format "\nDIVIDE: FuncCall ~a, return-place ~a, type ~a\n" 
                                      (send ast to-string) return-place type)))

        
        (define (func-args-at ast core)
          (filter 
	   (lambda (x) (= (get-field place-type x) core))
	   (get-field stmts (get-field args ast))))

	(define (new-funccall core)
          (when debug
                (pretty-display `(new-funccall ,core)))
	  (define params (func-args-at (get-field signature ast) core))
          (define return (if (list? return-place)
                             (filter (lambda (x) (= (get-field place-type x) core))
                                     return-place)
                             (if (and return-place (= return-place core))
                                 return-place
                                 #f)))
          (define new-return-place (and (not (empty? return)) return))
          
          (define (get-args n)
            (if (= n 0)
                (list)
                (let* ([top (pop-stack core)]
                       [place-type (get-field place-type top)]
                       [return-count (if (list? place-type) (length place-type) 1)])
                  (when debug
                        (pretty-display `(get-args n ,n top ,(send top to-string) 
                                                   count ,return-count)))
                  (cons top (get-args (- n return-count))))))

          (new FuncCall% [name name]
               ;; reverse order because we pop from stack
               [args (reverse (get-args (length params)))]
               [place-type new-return-place]
               [fixed-node (get-field fixed-node ast)]
               [type (if new-return-place type "void")]
               ))

        ;; prepare for actor mode
        (define actors-info (hash-has-key? actors name))
        (define need-setup #f)
        (define need-cleanup (list))
        (when
         actors-info
         (set! actors-info (hash-ref actors name))
         (set! need-setup (not (member name visited-actor-funcs)))
         (pretty-display `(ACTOR ,name ,need-setup ,visited-actor-funcs))
         
         (when
          need-setup
          (set! visited-actor-funcs (cons name visited-actor-funcs))
          ;; 1. set up actor: starting function.
          (for ([pair actors-info])
               (let* ([actor (car pair)]
                      [caller (cdr pair)]
                      [path (vector-2d-ref routing-table caller actor)]
                      [wire (take path (sub1 (length path)))])
                 (pretty-display `(set-workspace-actor ,actor))
                 (set-workspace-actor actor (last wire))))

          ;; 2. set up wiring node & caller
          (for ([i (hash-ref actors*-no-cf-map name)])
               (when (add-while i)
                     (pretty-display `(add-while ,i))
                     (set! need-cleanup (cons i need-cleanup))))
          )
         
         ;; 3. set up path
         (define (gen-comm-actor path)
           (when (>= (length path) 3)
                 (let ([a (car path)]
                       [b (cadr path)]
                       [c (caddr path)])
                   (push-workspace b (transfer a b c))
                   (gen-comm-actor (cdr path)))))
         ;; wiring nodes can be actors themselves, but that's okay
         ;; (for ([pair (topo-sort actors-info)])
         ;;      (let* ([actor (car pair)]
         ;;             [caller (cdr pair)]
         ;;             [path (vector-2d-ref routing-table caller actor)]
         ;;             [wire (drop (take path (sub1 (length path))) 1)]
         ;;             )
         ;;        ;; caller initiates
         ;;        ;; (pretty-display `(caller-actor ,caller ,wire ,actor))
         ;;        ;; (pretty-display `(add-remote-exec ,caller))
         ;;        (set! need-cleanup (cons actor need-cleanup))
         ;;        (add-remote-exec caller (second path) actor)
         ;;        (when
         ;;         (not (member pair visited-actor-pairs))
	 ;;         (set! visited-actor-pairs (cons pair visited-actor-pairs))
         ;;         ;; set up while loop in the wiring nodes
         ;;         (for ([i wire])
         ;;              ;; (pretty-display `(add-while ,i))
         ;;              (when (add-while i)
         ;;                    (set! need-cleanup (cons i need-cleanup))))
         ;;         ;; wiring nodes pass exec command to actor
         ;;         ;; (pretty-display `(gen-comm-actor ,path))
         ;;         (gen-comm-actor path))
         ;;        ))

         ;; 4. remote execuation path
         (for ([pair (topo-sort actors-info)])
              (let* ([actor (car pair)]
                     [caller (cdr pair)]
                     [path (vector-2d-ref routing-table caller actor)]
                     )
                ;; caller initiates
                ;; (pretty-display `(caller-actor ,caller ,actor))
                ;; (pretty-display `(add-remote-exec ,caller))
                (set! need-cleanup (cons actor need-cleanup))
                (add-remote-exec caller (second path) actor)
                (when
                 (not (member pair visited-actor-pairs))
		 (set! visited-actor-pairs (cons pair visited-actor-pairs))
                 (gen-comm-actor path))
                ))
         
         ) ;; end actor

        (when (or (regexp-match #rx"set_io" name)
                  (regexp-match #rx"digital_read" name)
                  (regexp-match #rx"digital_wakeup" name)
                  (regexp-match #rx"delay_ns" name)
                  (regexp-match #rx"delay_unext" name))
              (set! is-io-arg #t))
	;; add expressions for arguments
        (for ([arg (get-field args ast)])
             (send arg accept this))
        (set! is-io-arg #f)

        (let* ([place (get-field place-type ast)]
	       [sig (get-field signature ast)]
	       [type (if (get-field return sig)
                         (get-field type (get-field return sig))
                         "void")])

          (when debug 
                (pretty-display (format "\nDIVIDE: FuncCall ~a, sig=~a\n" 
                                        (send ast to-string) (get-field body-placeset sig))))
          (for ([c (get-field body-placeset sig)])
	       ;; body-placeset of IO function is empty
               (let ([func (new-funccall c)])
                 (if (equal? (get-field type func) "void") ;(not (get-field place-type func))
                     ;; if place-type is empty, funcall is statement
                     (push-workspace c func)
                     ;; if place-type is not empty, funccall is exp
                     (push-stack-temp c func)
                     ))))
        (gen-comm)

        ;; clean up actor mode
        (when
         need-setup
         ;; prevent from using these nodes to do more computation.
         (for ([i (list->set need-cleanup)])
              (pretty-display `(clear-workspace-actor ,i))
              (clear-workspace i)))
        ]

       [(is-a? ast ArrayDecl%)
	(when debug 
	      (pretty-display (format "\nDIVIDE: ArrayDecl\n")))

        (define (sub-list lst from to)
          (and lst (take (drop lst from) (- to from))))

	(let ([place (if (get-field ghost ast) 
                         (get-field ghost ast)
                         (get-field place-list ast))])
	  (if (number? place)
	      (push-workspace place ast)
              ;; Doesn't work with int[]@{[0:10]=0,[10:20]=1,[20:30]=0}
	      (for ([p place])
		   (let ([here (get-field place p)]
                         [to (get-field to p)]
                         [from (get-field from p)])
		     (when debug
			   (pretty-display `(array ,(get-field var ast) ,here 
						   ,(get-field from p)
						   ,(get-field to p))))
		     (push-workspace 
		      here
		      (new ArrayDecl% [var (get-field var ast)]
			   [type (get-field type ast)]
			   [known (get-field known ast)]
			   [bound (- to from)]
                           [offset from]
                           [init (sub-list (get-field init ast) from to)]
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
	  (for ([here (list->set (map (lambda (x) (get-field place x)) place))])
               (push-workspace 
                here
                (new VarDecl% [var-list (get-field var-list ast)]
                     [type (get-field type ast)]
                     [known (get-field known ast)]
                     [place here])))]

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
            (when debug
                  `(INDEX-VEC ,index-vec))
	    (for ([name (get-field var-list ast)])
		 (dict-set! expand-map name index-vec)))
	  ])]

       [(is-a? ast Assign%) 
	(define lhs (get-field lhs ast))
	(define rhs (get-field rhs ast))

        (send rhs accept this)
	(set! is-lhs #t)
	(send lhs accept this)
	(set! is-lhs #f)

	(when debug (pretty-display (format "\nDIVIDE: Assign\n")))
        (let ([place (get-field place-type (get-field lhs ast))])
          (if (number? place)
              (begin
		;; pop left before right
                (set-field! rhs ast (pop-stack place))
                (push-workspace place ast))
	      ;; _temp1 = bbb(); tuple type
	      (let ([place-list (get-field place-list place)])
		(for ([p (list->set place-list)])
		     (let ([occur (count (lambda (x) (= x p)) place-list)])
		       (for ([i (in-range occur)])
			    (push-workspace p (new Assign% 
						   ;; pop left before right
						   [lhs (new Temp% [name (get-field name lhs)]
							     [place-type p]
							     [type (get-field type lhs)]
							     [compact (> occur 1)]
							     [sub (and (> occur 1) (- occur i 1))])]
						   [rhs (pop-stack p)]))))))))
	]

       [(is-a? ast Return%)
        (when debug (pretty-display (format "\nDIVIDE: Return\n")))
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
              (push-workspace place (new Return% [val (pop-stack place)]))))]

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
              (pretty-display `(visit-false))
              (send (get-field false-block ast) accept this))
              
	(when debug (pretty-display (format "\nDIVIDE: If (cleaning)\n")))
	;; pop scope
        (for ([c (get-field body-placeset ast)])
             (let* ([false-block (get-workspace c)]
                    [_ (pretty-display `(false-block ,c ,false-block))]
                    [if (get-field parent false-block)]
		    [old-workspace (get-field parent if)])
               (clear-stack c)
               (reverse-stmts false-block)
               (set-workspace c old-workspace)

	       ;; remove new-ast if its true-block and false-block are empty
	       (when (and (empty? (get-field stmts (get-field true-block if)))
			  (empty? (get-field stmts false-block)))
		     (set-field! stmts old-workspace (cdr (get-field stmts old-workspace))))))
	(when debug (pretty-display (format "\nDIVIDE: If (done)\n")))
	]
               
       [(is-a? ast While%)
	(when debug (pretty-display (format "\nDIVIDE: While\n")))
        ;; (send (get-field condition ast) accept this)
        ;; (gen-comm-condition)
        (scope-pattern 
         (lambda (c) 
	   (get-new-while ast
			  #f ;; condition
			  (new Block% [stmts (list)]) ;; body
			  #f #f;; bound, body-placeset pre
			  (new Block% [stmts (list)]) ;; pre
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

             [(not return) #f]

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
                  #f)
                  ;; (new VarDecl% 
                  ;;  [var-list (list "#return")]
                  ;;  [type "void"] [place core] [known (get-field known return)]))
              ]

             [(and (not (equal? (get-field type return) "void"))
                     (= (get-field place return) core))
              return]

             [else
              #f
              ;; (raise (format "visitor-divider: funcdecl: unimplemented for return type = ~a, place = ~a" (get-field type return) (get-field place return)))
              ]

             ;; [else
             ;;  (new VarDecl% 
             ;;       [var-list (list "#return")]
             ;;       [type "void"] [place core] [known (get-field known return)])]
             )))
                       
        (scope-pattern 
         (lambda (c)
           (let* ([return (func-return-at ast c)]
                  [func (new FuncDecl%
                            [name (get-field name ast)]
                            [args (func-args-at ast c)]
                            [return return]
                            [body (new Block% [stmts (list)])]
                            [parent (get-workspace c)])])
             ;; (pretty-display (format "FUNDECL: name ~a core ~a return ~a" 
             ;;                         (get-field name ast) c return))
             ;; (when return
             ;;       (pretty-display (format "type = ~a, place = ~a"
             ;;                               (get-field type return) (get-field place return))))
             (set-func c func)
             func)))]
         

       [(is-a? ast Program%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))
        (for ([i (in-range n)])
             (unless
              (is-a? (get-workspace i) Program%)
              (pretty-display (format "Top level scope @core ~a is not Program!" i))
              (pretty-display (get-workspace i))
              (send (get-workspace i) pretty-print)
              (raise (format "Top level scope @core ~a is not Program!" i)))
             (reverse-workspace i)
             ;;(pretty-display `(program ,i ,(get-field stmts (get-workspace i))))
             )
        
        (define programs (make-vector n))
        (for ([i (in-range n)])
             (vector-set! programs i (get-workspace i)))

        (for ([i (in-range n)])
             (let* ([program (vector-ref programs i)]
                    [stmts (get-field stmts program)])
               
               ;; Move port-listen inside main.
               (let ([listen
                      (findf (lambda (x) (is-a? x PortListen%)) stmts)]
                     [main
                      (findf (lambda (x)
                               (and (is-a? x FuncDecl%)
                                    (equal? (get-field name x) "main")))
                             stmts)])
                 (cond
                  [(and main listen)
                   (let ([main-body (get-field body main)])
                     (set-field! stmts main-body
                                 (append (get-field stmts main-body)
                                         (list listen)))
                     (set-field! stmts program (remove listen stmts)))]
                  [listen
                   (set-field! set-p program (get-field port listen))
                   ]

                  [(and (not main)
                        (findf (lambda (x) (is-a? x FuncDecl%)) stmts))
                   (let* ([last-funcdecl
                          (findf (lambda (x) (is-a? x FuncDecl%))
                                 (reverse stmts))]
                          [main-stmts
                           (list
                            (new FuncCall%
                                 [name (get-field name last-funcdecl)]
                                 [args (list)] [type "void"])
                            (new FuncCall%
                                 [name "main"]
                                 [args (list)] [type "void"]))]
                          [main-body (new Block% [stmts main-stmts])]
                          [main-func
                           (new FuncDecl% [name "main"]
                                [args (new Block% [stmts (list)])]
                                [return #f]
                                [body main-body])])
                     (set! stmts (append stmts (list main-func)))
                     (set-field! stmts program stmts))
                   ]

                  ))

               ;; (pretty-display `(PROGRAM ,i ,program))
               
               (unless
                (findf
                 (lambda (x)
                   (let ([name (and (is-a? x FuncDecl%)
                                    (get-field name x))])
                     (and name (or (equal? name "main")
                                   (regexp-match #rx"act" name)))))
                 stmts)
                (set-field! stmts program (list)))
               ))
        
        ;; Adjust offset
        (for ([i (in-range n)])
             (send (vector-ref programs i) accept (new offset-modifier%)))

        programs
	]

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]
       
       [else (raise (format "visitor-divider: unimplemented for ~a" ast))]

       ))))
