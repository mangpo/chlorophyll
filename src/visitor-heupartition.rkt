#lang racket

(require "header.rkt" "ast.rkt" "visitor-interface.rkt" "space-estimator.rkt")

(provide heuristic-partitioner% merge-sym-partition)

;;(define factor 0.95)

(define debug #f)

(define (merge-sym-partition n space flow-graph capacity 
			     refine-capacity part2capacity
			     conflict-list my-ast #:name [name #f])
  (define sol-map (make-hash))
  
  (define fixed (make-hash))
  (for ([pair (get-field fixed-parts my-ast)])
       (hash-set! fixed (car pair) (cdr pair)))
  (for ([pair (get-field module-inits my-ast)]
        [cluster-id (in-naturals)])
       (for ([p (car pair)])
            (hash-set! fixed p (- (add1 cluster-id)))))
  
  ;; Mapping from logical core (either concrete or symbolic) to physical core.
  (define node-to-core (make-hash))
  (when debug (pretty-display `(conflict-list ,conflict-list)))

  (define conflict-index (make-hash))
  (for ([conflict-id (in-naturals)]
        [group-list conflict-list])
       (for ([group-id (in-naturals)]
             [group group-list])
            (for ([p group])
                 (if (hash-has-key? conflict-index p)
                     (hash-set! conflict-index p
                                (cons (cons conflict-id group-id)
                                      (hash-ref conflict-index p)))
                     (hash-set! conflict-index p
                                (list (cons conflict-id group-id)))))))
  (when debug (pretty-display `(conflict-index ,conflict-index)))

  (define (conflict? x y)
    (define conflict #f)
    (when
     (and (hash-has-key? conflict-index x) (hash-has-key? conflict-index y))
     (let ([indexes-x (hash-ref conflict-index x)]
           [indexes-y (hash-ref conflict-index y)]
           [conflict #f])
       (for* ([index-x indexes-x]
              [index-y indexes-y] #:break conflict)
             (when (and (= (car index-x) (car index-y))
                        (not (= (cdr index-x) (cdr index-y))))
                   (set! conflict #t)))))

    (when (and (not conflict)
               (hash-has-key? fixed x)
               (hash-has-key? fixed y))
          (set! conflict (not (equal? (hash-ref fixed x) (hash-ref fixed y)))))
    conflict)

  (define (update-conflict child parent)
    (define c-conflict
      (if (hash-has-key? conflict-index child)
          (hash-ref conflict-index child)
          (list)))
    (define p-conflict
      (if (hash-has-key? conflict-index parent)
          (hash-ref conflict-index parent)
          (list)))
    (define my-conflict (append p-conflict c-conflict))
    (unless (empty? my-conflict) (hash-set! conflict-index parent my-conflict)))

  (define (root place)
    (define parent (hash-ref sol-map place))
    (if (equal? place parent)
        place
        (let ([r (root parent)])
          (hash-set! sol-map place r)
          r)))
  
  (define (point r1 r2)
    (when (hash-has-key? fixed r1)
          (hash-set! fixed r2 (hash-ref fixed r1)))
    (hash-set! sol-map r1 r2)
    (hash-set! space r2 (+ (hash-ref space r1) (hash-ref space r2)))
    (hash-remove! space r1))
  
  (define (unify p1 p2 [scale 1] #:must [must #f])
    (when debug (pretty-display `(unify ,p1 ,p2)))
    (define r1 (root p1))
    (define r2 (root p2))
    (define c1 (and (hash-has-key? node-to-core r1) (hash-ref node-to-core r1)))
    (define c2 (and (hash-has-key? node-to-core r2) (hash-ref node-to-core r2)))

    (define limit capacity)
    (define parent #f)
    (define child #f)
    (when (hash-has-key? part2capacity r1)
          (set! limit (hash-ref part2capacity r1))
          (set! parent r1)
          (set! child r2)
          )
    (when (and (hash-has-key? part2capacity r2)
               (< (hash-ref part2capacity r2) limit))
          (set! limit (hash-ref part2capacity r2))
          (set! parent r2)
          (set! child r1)
          )

    (when (symbolic? r1)
          (set! parent r2)
          (set! child r1))

    (when (symbolic? r2)
          (set! parent r1)
          (set! child r2))

    (unless parent
            (set! parent r1)
            (set! child r2))
    
    (when (and (not (equal? r1 r2))
               (or must
                   (and
                    (or (symbolic? r1) (symbolic? r2))
                    (or (not c1) (not c2) (equal? c1 c2))
                    (not (conflict? r1 r2))
                    (< (+ (hash-ref space r1) (hash-ref space r2)) 
                       (inexact->exact (floor (* limit scale)))))))
          (cond
           [(and c1 (not c2)) (hash-set! node-to-core r2 c1)]
           [(and c2 (not c1)) (hash-set! node-to-core r1 c2)])

          ;;(when debug (pretty-display `(root ,r1 ,r2 ,limit)))
          (when
           debug
           (pretty-display (format "merge ~a->~a + ~a->~a = parent ~a"
                                   p1 r1 p2 r2 parent))
           (pretty-display (format "space ~a ~a"
                                   (hash-ref space r1) (hash-ref space r2))))
          (update-conflict child parent)
          (point child parent)
          )
    )


  (when debug
        (pretty-display "------------------ before merge sym partition ---------------------")
        (pretty-display space)
        (pretty-display `(flow-graph ,flow-graph ,(list? flow-graph))))

  ;; point to itself
  (for ([key (hash-keys space)])
       (hash-set! sol-map key key))

  ;; (pretty-display `(space-map ,space))

  ;; initialize node-to-core
  (for ([pair (hash->list node-to-symbolic-core)])
       (let ([core (car pair)]
             [node (cdr pair)])
         (hash-set! node-to-core node core)))

  ;; unify concrete fixed node to its symbolic node
  (for ([mapping (get-field fixed-parts my-ast)])
       (let ([node (car mapping)]
             [core (cdr mapping)])
         (when (hash-has-key? node-to-symbolic-core core)
               (let ([sym-core (hash-ref node-to-symbolic-core core)])
                 (when (hash-has-key? sol-map sym-core)
                       (pretty-display `(fixed-unify ,node ,core ,sym-core))
                       (unify node (hash-ref node-to-symbolic-core core)
                              #:must #t))))))
  
  (for ([e flow-graph])
       (unify (car e) (cdr e)))

  ;; post merge to reduce number of cores
  (when #t ;;(> (hash-count space) (- n 3))
        (define partitions (hash-keys space))
        (for* ([p1 partitions]
               [p2 partitions])
              (when (and (hash-has-key? space p1) (hash-has-key? space p2))
                    (unify p1 p2 0.5))))

  (define vals (hash-values sol-map))
  (define concrete-vals (list->set (filter (lambda (x) (not (symbolic? x))) vals)))
  (when debug (pretty-display `(concrete-vals ,concrete-vals)))
  (define counter 0)
  (define (next-counter)
    (if (set-member? concrete-vals counter)
        (begin
          (set! counter (add1 counter))
          (next-counter))
        counter))
  ;; (pretty-display `(sol-map ,sol-map))

  (define concrete2sym (make-vector n #f))
  (for ([key (hash-keys sol-map)])
       (let ([val (root key)])
         (if (symbolic? val)
	     (begin
               (hash-set! sol-map val (next-counter))
	       ;; (pretty-display `(set-concrete2sym ,counter val))
	       (vector-set! concrete2sym counter val) ;; set concrete2sym
	       ;; (when debug (pretty-display `(hash-set ,val ,counter)))
	       (hash-set! sol-map counter counter)
	       (set! counter (add1 counter))
	       (root key))
	     (when (equal? (vector-ref concrete2sym val) #f)
		   (vector-set! concrete2sym val val))))) ;; set concrete2sym

  (with-output-to-file #:exists 'truncate (format "~a/~a.space" outdir name)
    (lambda ()
      (pretty-display "========== SPACE ============")
      (for ([pair (hash->list space)])
           (let ([p (car pair)]
                 [s (cdr pair)])
             (pretty-display (format "~a ~a" (hash-ref sol-map p) s))))))
         
  
  (when debug
        (pretty-display "------------------ after merge sym partition ---------------------")
        (pretty-display `(sol-map ,sol-map)))
  (cons sol-map concrete2sym))
  

(define heuristic-partitioner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [space (make-hash)])

    (define function-network (make-hash))
    (define actors #f)
    (define actors* #f)
    
    ;; Declare IO function: in(), out(data)
    (hash-set! function-network "in" (cons (list) (set)))
    (hash-set! function-network "out" (cons (list) (set)))

    (for ([node (append analog-nodes digital-nodes)])
      (for ([name '("digital_read~a"
                    "set_io~a"
                    "digital_wakeup~a"
                    "delay_ns~a"
                    "delay_unext~a")])
        (hash-set! function-network (format name node)
                   (cons (list) (set)))))

    (define (inc-space place sp)
      ;;(pretty-display `(inc-space ,place))
      (when
       place
       (if (is-a? place TypeExpansion%)
           (for ([p (get-field place-list place)])
                (inc-space p sp))
           (if (hash-has-key? space place)
               (hash-set! space place (+ (hash-ref space place) sp))
               (hash-set! space place sp)))))

    (define (inc-space-placeset places sp)
      (for ([p places]) (inc-space p sp)))

    (define (network p1 p2)
      (when debug (pretty-display `(network ,p1 ,p2)))
      (cond
       [(and (is-a? p1 TypeExpansion%) (is-a? p2 TypeExpansion%))
	(flatten
	 (for/list ([x (get-field place-list p1)]
		    [y (get-field place-list p2)])
		   (network x y)))]

       [(is-a? p1 TypeExpansion%)
        (network (car (get-field place-list p1)) p2)]

       [(is-a? p2 TypeExpansion%)
        (network (car (get-field place-list p2)) p1)]

       [(and (not (equal? p1 p2)) 
             ;; (not (equal? p1 (* 2 w)))
             ;; (not (equal? p2 (* 2 w)))
             (rosette-number? p1)
             (rosette-number? p2)
             (or (symbolic? p1) (symbolic? p2)))
        (when debug (pretty-display `(network-real ,p1 ,p2)))
	(list (cons (cons p1 p2) 1))
        ]

       [else (list)]))

    (define (multiply-weight graph w)
      (map (lambda (e) (cons (car e) (* (cdr e) w))) graph))

    (define (graph->list graph)
      (define h (make-hash))
      (for ([edge graph])
	   (let* ([e1 (car edge)]
		  [e2 (cons (cdr e1) (car e1))]
		  [w (cdr edge)])
	     (cond
	      [(hash-has-key? h e1)
	       (hash-set! h e1 (+ (hash-ref h e1) w))]
	      
	      [(hash-has-key? h e2)
	       (hash-set! h e2 (+ (hash-ref h e2) w))]

	      [else
	       (hash-set! h e1 w)])))

      (hash->list h))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (when debug 
              (pretty-display (format "HUE: Num ~a" (send ast to-string))))
        (when (get-field place-type ast)
          (inc-space (get-field place-type ast) est-num))
        (list)
        ]

       [(is-a? ast Array%)
        (when debug 
              (pretty-display (format "HUE: Array ~a" (send ast to-string))))
        (define index (get-field index ast))
        (define place-type (get-field place-type ast))
        (inc-space place-type est-acc-arr)

        ;; Infer place
        (send index infer-place place-type)
        (define index-ret (send index accept this))
        
        (append index-ret
                (network place-type (get-field place-type index)))
        ]

       [(is-a? ast Var%)
        (when debug 
              (pretty-display (format "HUE: Var ~a" (send ast to-string))))
        (inc-space (get-field place-type ast) est-var)
        (list)
        ]

       [(is-a? ast UnaExp%)
        (when debug 
              (pretty-display (format "HUE: UnaExp ~a" (send ast to-string))))
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        
        ;; set place-type
        (define place-type (get-field place-type ast))
        
        ;; Infer place
        (send e1 infer-place place-type)
        (define e1-ret (send e1 accept this))
        
        (inc-space place-type (hash-ref space-map (get-field op op)))
        (append e1-ret
                (network place-type (get-field place-type e1)))
        ]

       [(is-a? ast BinExp%)
        (when debug
              (pretty-display (format "HEU: BinExp% ~a, place-type = ~a" 
                                      (send ast to-string) (get-field place-type ast))))
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        
        ;; set place-type
        (define place-type (get-field place-type ast))
        (send e1 infer-place place-type)
        (send e2 infer-place place-type)
        (define e1-ret (send e1 accept this))
        (define e2-ret (send e2 accept this))
        
        (inc-space place-type (hash-ref space-map (get-field op op)))
        (append e1-ret e2-ret
                (network place-type (get-field place-type e1))
                (network place-type (get-field place-type e2)))
        ]

       [(is-a? ast FuncCall%)
        (when debug 
              (pretty-display (format "HEU: FuncCall(1) ~a, sig=~a" 
				      (send ast to-string)
				      (get-field signature ast))))
        (define name (get-field name ast))
        (define networks-places (hash-ref function-network name))
        (define networks (car networks-places))
        (define body-places (cdr networks-places))

        (if (io-func? name)
            (inc-space (hash-ref node-to-symbolic-core (get-field fixed-node ast))
                       (get-built-in-space name))
            ;;(inc-space-placeset (filter-out body-places name) est-funccall)
            (void)
            )

        ;; infer place-type
	(for ([param (get-field stmts (get-field args (get-field signature ast)))]
	      [arg (flatten-arg (get-field args ast))])
	     (when #t 
		   (pretty-display (format "HEU: FuncCall(2) ~a param=~a, arg=~a" name param arg)))
             (when
              (and (not (hash-has-key? actors name))
                   (not (hash-has-key? actors* name)))
              (when #t (pretty-display (format "HEU: infer!")))
              (send arg infer-place (get-field place-type param)))
             ;; (if
             ;;  (or (hash-has-key? actors name)
             ;;      (hash-has-key? actors* name))
             ;;  (when (and (is-a? arg Num%) (not (get-field place-type arg)))
             ;;        (send arg set-place-sym))
             ;;  (begin (pretty-display (format "HEU: infer!"))
             ;;         (send arg infer-place (get-field place-type param))))
	     (set! networks 
		   (append networks
			   (network (get-field place-type arg) (get-field place-type param)))))
        ;; visit children
        (for ([arg (get-field args ast)])
	     (let ([arg-ret (send arg accept this)])
	       (set! networks (append networks arg-ret))))
        (when debug 
              (pretty-display (format "HUE: FuncCall(3) ~a" (send ast to-string))))
        networks
        ]

       [(is-a? ast Assign%)
        (when debug
              (pretty-display (format "HEU: Assign% ~a = ~a" 
                                      (send (get-field lhs ast) to-string)
                                      (send (get-field rhs ast) to-string))))
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))

        ;; infer place
        (send rhs infer-place (get-field place-type lhs))
        (send lhs infer-place (get-field place-type rhs))

        (define rhs-ret (send rhs accept this))
        (define lhs-ret (send lhs accept this))

        (append lhs-ret rhs-ret
                (network (get-field place-type lhs) (get-field place-type rhs)))
        ]

       [(is-a? ast VarDecl%)
        (define place (get-field place ast))
        (inc-space place (* (length (get-field var-list ast))
                            (if (is-a? ast Param%)
                                (add1 est-data1)
                                est-data1)))
        (list)
        ]

       [(is-a? ast ArrayDecl%)
        (define place-list (get-field place-list ast))
        (define last 0)
        (for ([p place-list])
             (let ([from (get-field from p)]
                   [to   (get-field to p)])
               (when (not (= from last))
                     (send ast bound-error))
               (set! last to)
               (inc-space (get-field place p) (* (- to from) est-data2)) ; increase space
               ))

        (when (not (= (get-field bound ast) last))
              (send ast bound-error))
	(list)
        ]

       [(is-a? ast For%)
        (define place-list (get-field place-list ast))
              
        (define last 0)
        
        (when (list? place-list)
              (for ([p place-list])
                   (let* ([from (get-field from p)]
                          [to   (get-field to p)])
                     (when (not (= from last))
                           (send ast bound-error))
                     (set! last to))))

        ;;(inc-space-placeset (get-field body-placeset ast) est-for)

        (define body-ret (send (get-field body ast) accept this))
        (multiply-weight body-ret (- (get-field to ast) (get-field from ast)))
        ]

       [(is-a? ast If%)
        (define condition (get-field condition ast))
        (define true-block (get-field true-block ast))
        (define false-block (get-field false-block ast))
        
        (define cond-place (get-field place-type condition))
        (define body-place (get-field body-placeset true-block))
        
        (define cond-ret (send condition accept this))
        (define body-ret (send true-block accept this))
        (when false-block
	      (define false-ret (send false-block accept this))
	      (set! body-ret (append body-ret false-ret))
              (set! body-place
                    (set-union body-place
                               (get-field body-placeset false-block))))

	(define networks (append cond-ret body-ret))
        (for ([b body-place])
             (set! networks (append networks (network cond-place b))))

        ;; increase space
        (inc-space cond-place est-comm) 
        ;;(inc-space-placeset (get-field body-placeset ast) est-if)

	networks
        ]

       [(is-a? ast While%)
        (define cond-place (get-field place-type (get-field condition ast)))
        (define body-place (get-field body-placeset (get-field body ast)))
        (define pre-place (get-field body-placeset (get-field pre ast)))
        
        (define pre-ret (send (get-field pre ast) accept this))
        (define cond-ret (send (get-field condition ast) accept this))
        (define body-ret (send (get-field body ast) accept this))

	(define networks (append cond-ret body-ret pre-ret))
        (for ([b (set-union body-place pre-place)])
             (set! networks (append networks (network cond-place b))))
          
        ;; increase space
        (inc-space cond-place est-comm) 
        ;;(inc-space-placeset (get-field body-placeset ast) est-while)

        (multiply-weight networks (get-field bound ast))
        ]

       [(is-a? ast Return%) (list)]
       
       [(is-a? ast FuncDecl%)
        (when (get-field return ast)
              (send (get-field return ast) accept this))
	(define args-ret (send (get-field args ast) accept this))
        (define body-ret (send (get-field body ast) accept this))
	(hash-set! function-network (get-field name ast)
		   (cons (append args-ret body-ret)
                         (get-field body-placeset ast)))

	(list)
	]

       [(is-a? ast Program%)
        (set! actors (get-field actors ast))
        (set! actors* (get-field actors* ast))
        
	(for/list ([stmt (get-field stmts ast)])
		  (send stmt accept this))

        (define sorted-edges (sort (graph->list
                                    (car (hash-ref function-network "main")))
				   (lambda (x y) (> (cdr x) (cdr y)))))
        ;;(pretty-display `(sorted-edges ,sorted-edges))
        (values space (map car sorted-edges) (get-field conflict-list ast))]

       [(is-a? ast Block%)
        (foldl (lambda (stmt all) 
                 (append all (send stmt accept this)))
               (list) (get-field stmts ast))]

       [else
        (raise (format "visitor-heupartition: unimplemented for ~a" ast))]
       ))))

      
