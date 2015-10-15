#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "ast-util.rkt"
         "parser.rkt" 
	 "partition-storage.rkt"
         "space-estimator.rkt"
         "visitor-interface.rkt" 
         "visitor-desugar.rkt")

(provide count-msg-interpreter% (struct-out comminfo))

(struct comminfo (msgs placeset))

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field places
                [env (make-hash)] ;; map function name -> comminfo
                [has-func-temp #f]
		[used-io-nodes (set)]
		)

    (define debug #f)
    (define debug-sym #f)
    
    ;; Declare IO function: in(), out(data)
    (declare env "in" (comminfo 0 (set)))
    (declare env "out" (comminfo 0 (set)))

    (for ([node io-nodes])
      (for ([name built-in-names])
        (declare env (format "~a~a" name node)
                 (comminfo 0 (set (hash-ref node-to-symbolic-core node))))))

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      ;(assert (or (number? place) (list? place)))
      (cond
        [(number? place)
         (cores-inc-space places place add-space)]
        
        [(at-io? place)
         void]
        
        [(is-a? place TypeExpansion%)
         (for ([p (get-field place-list place)])
              (cores-inc-space places p add-space))]
        
        [else
         (pretty-display `(inc-space ,place))
         (raise "inc-space")
         (let ([place-list (if (place-type-dist? place) (car place) place)])
           (for ([p place])
		(cores-inc-space places (get-field place p) add-space)))]))
    
    ;;; Increase the used space of "place" with op.
    (define (inc-space-with-op place op)
      (assert (or (number? place) (list? place)))
      ;TODO: change to (cores-add-op places place op)
      (if (number? place)
          (cores-inc-space places place (est-space op))
          (let ([place-list
                 (if (place-type-dist? place) (car place) place)])
            (raise "inc-space-with-op")
            (for ([p place-list])
                 (cores-inc-space places (get-field place p) (est-space op))))))
    
    ;; Increase the used space of places in the given set by "add-space".
    ;; Used in body inside for and if.
    (define (inc-space-placeset placeset add-space)
      (define (loop placelist)
        (unless (empty? placelist)
          (let* ([others (cdr placelist)]
                 [me (car placelist)])
            (loop others)
            (when (andmap (lambda (x) (not (equal? x me))) others) 
                  (inc-space me add-space)))))
        
      (loop (set->list placeset)))

    (define (inc-space-return returns)
      (define (loop placelist)
        (unless (empty? placelist)
          (let* ([others (cdr placelist)]
                 [me (car placelist)])
            (loop others)
            (when (andmap (lambda (x) (not (equal? x me))) others)
                  (let ([occur (count (lambda (x) (equal? x me)) returns)])
                    (when (>= occur 2)
                          (when debug
                                (pretty-display 
                                 `(inc-space-return ,me ,(* occur est-funcreturn))))
                          (inc-space me (* occur est-funcreturn))))))))
      (loop returns))

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg-place-type x y x-ast [y-ast #f])
      (when debug
      	    (pretty-display `(count-msg-place-type ,x ,y)))
      ;(assert (and (is-a? x-ast Base%) (is-a? y-ast Base%)))

      ;; Return the place that a resides if a only lives in one place. 
      ;; Otherwise, return string representation of a.
      ;; (define (get-str-rep a)
      ;;   ;(assert (place-type? a))
      ;;   (if (number? a)
      ;;       a
      ;;       (let ([place-list (car a)]
      ;;             [index (cdr a)])
      ;;         (if (= (length place-list) 1)
      ;;             (get-field place (car place-list))
      ;;             (format "~a . ~a" 
      ;;                     (place-list-to-string place-list)
      ;;                     (send index to-string))))))

      ;; Add comm space to cores
      (define (add-comm x)
	;(assert (place-type? x))
        (if (number? x)
             (inc-space x est-comm)
             (for ([p (car x)])
                  (raise "add-comm")
                  (inc-space (get-field place p) est-comm))))

      ;; Return 1 if it is in one place or it is a non-cluster array.
      ;; Return number of cores p resides in otherwise.
      (define (count-comm p p-ast)
	;(pretty-display `(count-comm ,p ,(send p-ast to-string)))
	;(assert (place-type? p))
        (cond
          [(or (number? p) (equal? p #f)) 1]
          [(is-a? p Place%)
           (let ([at (get-field at p)])
             (if (or (equal? at "any") (equal? at "io"))
                 1
                 (raise "count-comm doesn't support Place% that is not @any")))]
          [(pair? p)
           (raise "count-comm")
           (if (get-field cluster p-ast)
	       (length (car p))
	       1)]
          [else (raise (format "visiter-interpretor: count-comm: unimplemented for ~a" p))]))
      
      (define x-comm (count-comm x x-ast))
      
      (define y-comm (count-comm y y-ast))

      (cond 
        ;; if x and y are the same place.
        [(same-place? x y) 0]

	;; if x and y are in a single core.
	[(and (= x-comm 1) (= y-comm 1))
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display "COMM + 1"))
	 1]

        ;; if x or y is in multiple cores.
        [else 
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display (format "COMM + ~a + ~a" x-comm y-comm)))
         (+ x-comm y-comm)]))

    (define (count-msg x-ast y-ast)
      (when debug
	    (pretty-display `(count-msg ,(send x-ast to-string) ,(send y-ast to-string))))
      (count-msg-place-type (get-field place-type x-ast) (get-field place-type y-ast)
			    x-ast y-ast))

    (define (count-msg-placeset p-ast placeset)
      ;(when debug (pretty-display `(count-msg-placeset)))
      (define p (get-field place-type p-ast))
      (define (loop placelist)
        (if (empty? placelist)
            0
            (let* ([others (cdr placelist)]
                 [me (car placelist)])
              (+ (loop others)
                 (if (andmap (lambda (x) (not (equal? x me))) others)
                     (count-msg-place-type p me p-ast)
                     0)))))
      (loop (set->list placeset)))

    (define/public (evaluate-comminfo func-ast)
      (let* ([name (get-field name func-ast)]
             [ret  (cdr (dict-ref env name))])
            (dict-set! env name 
                       (cons func-ast (comminfo (evaluate-with-sol (comminfo-msgs ret))
                                                (comminfo-placeset ret))))))
                 
    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__")))

    (define/public (assert-conflict ast)

      (pretty-display `(conflict-list ,(get-field conflict-list ast)))
      
      (for ([lst (get-field conflict-list ast)])
	   (for* ([set-x lst]
		  [set-y lst])
		 (unless (equal? set-x set-y)
			 (for* ([x set-x]
				[y set-y])
			       (assert (not (= x y)))))))

        ;; unify concrete fixed node to its symbolic node
        (for ([mapping (get-field fixed-parts ast)])
             (let ([node (car mapping)]
                   [core (cdr mapping)])
               (when (hash-has-key? node-to-symbolic-core core)
                     (assert (= node (hash-ref node-to-symbolic-core core))))))

        ;; logical cores of different physical cores can't be the same.
        (define sym-nodes (list->vector (hash-values node-to-symbolic-core)))
        (for* ([i (vector-length sym-nodes)]
               [j (vector-length sym-nodes)])
              (unless (= i j)
                      (assert (not (= (vector-ref sym-nodes i)
                                      (vector-ref sym-nodes j))))))
      )
      
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (when debug (pretty-display (format ">> Num ~a" (send ast to-string))))
        (inc-space (get-field place-type ast) est-num) ; increase space
        0]
       
       [(is-a? ast Array%)
        (when debug (pretty-display (format ">> Array ~a" (send ast to-string))))
        (define index (get-field index ast))
        (define place-type (get-field place-type ast))
        ;; Infer place
        (send index infer-place place-type)
        (define index-ret (send index accept this))
        (inc-space place-type est-acc-arr) ; not accurate
        
        (when (and debug-sym (symbolic? (+ index-ret) (count-msg index ast)))
              (pretty-display (format ">> SYM Array ~a\n~a" 
                                      (send ast to-string)
                                      (+ index-ret (count-msg index ast)))))
        
        (+ index-ret (count-msg index ast))
        ]

       [(is-a? ast Var%)
        (when debug (pretty-display (format ">> Var ~a" (send ast to-string))))
        (inc-space (get-field place-type ast) est-var)
        0
        ]

       [(is-a? ast UnaExp%)
        (when debug (newline))
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        
        ;; set place-type
        (define place-type (get-field place-type ast))
        
        ;; Infer place
        (send e1 infer-place place-type)
        (define e1-ret (send e1 accept this))
        
        (inc-space place-type (hash-ref space-map (get-field op op))) ; increase space
        
        (when debug
              (pretty-display (format ">> UnaOp ~a" (send ast to-string))))
        (when (and debug-sym (symbolic? (+ e1-ret (count-msg ast e1))))
              (pretty-display (format ">> SYM UnaOp ~a\n~a" (send ast to-string)
                                      (+ e1-ret (count-msg ast e1)))))
        
        (+ e1-ret (count-msg ast e1))
        ]

       [(is-a? ast BinExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define op (get-field op ast))
          
          ;; set place-type
          (define place-type (get-field place-type ast))

          (when debug
                (pretty-display (format ">> BinOp ~a ~a (before)" (send ast to-string) place-type))
                (send ast pretty-print))

          ;; Infer place
          (send e1 infer-place place-type)
          (send e2 infer-place place-type)
          (define e1-ret (send e1 accept this))
          (define e2-ret (send e2 accept this))
          
	  (inc-space place-type (hash-ref space-map (get-field op op))) ; increase space

          (when debug
                (pretty-display (format ">> BinOp ~a ~a (after)" (send ast to-string) place-type))
                (send ast pretty-print))

          (when (and debug-sym
                 (symbolic? (+ e1-ret e2-ret
                               (count-msg ast e1)
                               (count-msg ast e2))))
                (pretty-display (format ">> SYM BinOp ~a\n~a" 
                                        (send ast to-string)
                                        (+ e1-ret e2-ret
                                           (count-msg ast e1)
                                           (count-msg ast e2)))))


          (+ e1-ret e2-ret
             (count-msg ast e1)
             (count-msg ast e2))
          ]

       [(is-a? ast FuncCall%)
        (define name (get-field name ast))
        (when (io-func? name)
          ;;add the node of this function to the set of used io nodes
          (set! used-io-nodes
            (set-add used-io-nodes (get-field fixed-node ast))))

          (when debug
                (pretty-display (format ">> FuncCall ~a" (send ast to-string))))

	  (define func-ret (lookup env ast))
          (define func-ast (get-field signature ast))
          (when debug
                (pretty-display (format ">> FuncCall ~a, signature = ~a" (send ast to-string) func-ast)))

          (define msgs (comminfo-msgs func-ret))
	  (define placeset (comminfo-placeset func-ret))

          ;; increase space
          (inc-space-placeset placeset (if (io-func? name)
                                           (get-built-in-space name)
                                           est-funccall))

          ;; infer place-type
          (for ([param (get-field stmts (get-field args func-ast))]
                [arg (flatten-arg (get-field args ast))])
               (send arg infer-place (get-field place-type param))
               )

          ;; visit children
	  (for ([arg (get-field args ast)])
               (let ([arg-ret (send arg accept this)])
                 (set! msgs (+ msgs arg-ret))))

          ;; count msg
	  (for ([param (get-field stmts (get-field args func-ast))]
                [arg (flatten-arg (get-field args ast))])
               ;; infer place-type
               (set! msgs (+ msgs (count-msg param arg)))
               )
          
	  (define return (get-field return func-ast))
          (when return
                (let ([expanded-return (typeexpansion->list (get-field place return))])
                  ;; remove this if it takes very long
                  (when (get-field might-need-storage ast)
                        (inc-space-return (map (lambda (x) (get-field place-type x))
                                               expanded-return)))))
		 
	  msgs]

       [(is-a? ast ReturnDecl%)
	;; no inc-space & return empty
	0
	]

       [(is-a? ast TempDecl%)
        (inc-space (get-field place ast) est-data)
        0]
                
       [(is-a? ast VarDecl%) 
        (define place (get-field place ast))
        (define var-list (get-field var-list ast))

        (when debug
              (pretty-display (format ">> VarDecl ~a ~a" var-list place)))
        
        (inc-space place (* (length var-list) 
                            (if (is-a? ast Param%)
                                (add1 est-data)
                                est-data))) ; increase space
        
        (when debug
              (pretty-display (format ">> VarDecl ~a (after)" var-list)))
        
        0
        ]

       [(is-a? ast ArrayDecl%)
        (define place-list (get-field place-list ast)) ; don't support place(x)
        (when debug
              (pretty-display (format ">> ArrayDecl ~a" (get-field var ast))))
        
        ;; check boundaries
        (define last 0)
        (for ([p place-list])
             (let* ([from (get-field from p)]
                    [to   (get-field to p)])
               (when (not (= from last))
                     (send ast bound-error))
               (set! last to)
               (inc-space (get-field place p) (* (- to from) est-data)) ; increase space
               ))
        
        (when (not (= (get-field bound ast) last))
              (send ast bound-error))
        
        0
        ]

       [(is-a? ast For%)
          (define place-list (get-field place-list ast)) ; don't allow place(x)

          (when debug
                (pretty-display (format ">> For ~a" (send (get-field iter ast) to-string))))

          ;; check boundaries
          (define last 0)
        
          (when (list? place-list)
              (for ([p place-list])
                   (let* ([from (get-field from p)]
                          [to   (get-field to p)])
                     (when (not (= from last))
                           (send ast bound-error))
                     (set! last to))))
		 ;(inc-space (get-field place p) est-for))) ; increase space

          ;; Add new scope for body.
          (push-scope)

          (define body-ret (send (get-field body ast) accept this))
          (define body-place-set (get-field body-placeset ast))
          
          (inc-space-placeset body-place-set est-for)

          ;; Remove scope.
          (pop-scope)

          (* body-ret (- (get-field to ast) (get-field from ast)))
          ]

       [(is-a? ast If%)
        (define condition (get-field condition ast))
        (define true-block (get-field true-block ast))
        (define condition-ret (send condition accept this))

        (push-scope)
        (define true-ret (send true-block accept this))
        (pop-scope)
        
        ;; between condition and true-block
        (define msgs (+ condition-ret true-ret))
        (define places (get-field body-placeset true-block))
        
        (define false-block (get-field false-block ast))
        (push-scope)

        (when false-block
              (define false-ret (send false-block accept this))
              (set! msgs (+ msgs false-ret))
              (set! places (set-union places (get-field body-placeset false-block))))
        (pop-scope)

        ;; increase space
        (inc-space-placeset places est-if)

	(when debug (pretty-display ">> FOR (count-msg-placeset)"))
        (+ msgs (count-msg-placeset condition places))
        ]

       [(is-a? ast While%)
          (define pre-ret (send (get-field pre ast) accept this))
          (define condition (get-field condition ast))
          (define condition-ret
            (if (is-a? condition Num%)
                (begin
                  (set-field! place-type condition (new Place% [at "any"]))
                  0)
                (send condition accept this)))
          
          (push-scope)
          (define body-ret (send (get-field body ast) accept this))
          (pop-scope)
          
          ;; increase space
          (define body-placeset (get-field body-placeset (get-field body ast)))
          (inc-space-placeset body-placeset est-while)

          (* (get-field bound ast)
             (+ condition-ret pre-ret body-ret)
             (count-msg-placeset condition body-placeset))
	  ]

       [(is-a? ast AssignTemp%)
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)]

       [(is-a? ast Assign%) 
        (when debug (newline))
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))
        
        (when debug
              (pretty-display ">> Assign (lhs)"))
        
        ;; infer place
        (send rhs infer-place (get-field place-type lhs))
        (send lhs infer-place (get-field place-type rhs))
        
        ;; Visit lhs
        (define lhs-ret (send lhs accept this))
        (define rhs-ret (send rhs accept this))
        
        (+ rhs-ret lhs-ret (count-msg lhs rhs))
        ]

       [(is-a? ast Return%)
	0]

       [(is-a? ast Program%)
        (when debug (pretty-display ">> Program"))

	(define ret #f)
	(for ([decl (get-field stmts ast)])
	     (let ([decl-ret (send decl accept this)])
	       (when (and (is-a? decl FuncDecl%)
                          (equal? (get-field name decl) "main"))
		     (set! ret decl-ret))))
	;; Return main declaration
	ret]

       [(is-a? ast Block%) 
        (when debug (pretty-display ">> Block"))
        (let ([ret
               (foldl (lambda (stmt all) 
                        (+ all (send stmt accept this)))
                      0 (get-field stmts ast))])
          
          (when (and debug-sym (symbolic? ret))
                (pretty-display `(BLOCK SYM num-msgs = ,ret)))
          ret)]

       [(is-a? ast FuncDecl%)
        (when debug
              (pretty-display (format ">> FuncDecl(1) ~a" (get-field name ast))))
        (push-scope)
        (define return (get-field return ast))
        (define return-ret 
          (if return
              (send return accept this)
              0))
        (define args-ret (send (get-field args ast) accept this))

        (define body-ret (send (get-field body ast) accept this))
        (pop-scope)
        
        (when debug
              (pretty-display (format ">> FuncDecl(2) ~a" 
                                      (get-field name ast))))

        (define body-placeset (get-field body-placeset ast))

        (let ([ret (+ args-ret body-ret return-ret)])
          ;; declare function
          (declare env (get-field name ast) (comminfo ret body-placeset))
          ret)]
		
       [else (raise (format "visitor-interpreter: unimplemented for ~a" ast))]))
))
