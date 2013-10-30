#lang racket

(require "header.rkt" "ast.rkt" "visitor-interface.rkt" "space-estimator.rkt")

(provide (all-defined-out))

(define factor 1)

(define (merge-sym-partition space flow-graph capacity)
  (define sol-map (make-hash))

  (define (root place)
    (define parent (hash-ref sol-map place))
    (if (equal? place parent)
        place
        (let ([r (root parent)])
          (hash-set! sol-map place r)
          r)))
  
  (define (point r1 r2)
    (hash-set! sol-map r1 r2)
    (hash-set! space r2 (+ (hash-ref space r1) (hash-ref space r2)))
    (hash-remove! space r1))
  
  (define (unify p1 p2)
    ;(pretty-display `(unify ,p1 ,p2))
    (define r1 (root p1))
    (define r2 (root p2))
    (when (and (not (equal? r1 r2))
               (or (symbolic? r1) (symbolic? r2))
               (< (+ (hash-ref space r1) (hash-ref space r2)) 
                  (inexact->exact (floor (* capacity factor)))))
          (if (symbolic? r1)
              (point r1 r2)
              (point r2 r1))))

  (pretty-display "------------------ before merge sym partition ---------------------")
  (pretty-display space)
  (pretty-display `(flow-graph ,flow-graph ,(list? flow-graph)))

  ;; point ot itself
  (for ([key (hash-keys space)])
       (hash-set! sol-map key key))

  (for ([e flow-graph])
       (unify (car e) (cdr e)))

  (define vals (hash-values sol-map))
  (define concrete-vals (set (filter (lambda (x) (not (symbolic? x))) vals)))
  (define counter 0)
  (define (next-counter)
    (if (set-member? concrete-vals counter)
        (begin
          (set! counter (add1 counter))
          (next-counter))
        counter))
  (define more-sol (make-hash))
  (for ([key-val (hash->list sol-map)])
       (let* ([key (car key-val)]
              [val (cdr key-val)])
         (when (symbolic? val)
               (unless (hash-has-key? more-sol val)
                       (hash-set! more-sol val (next-counter))
                       (set! counter (add1 counter)))
               (hash-set! sol-map key (hash-ref more-sol val)))))
  
  
  (pretty-display "------------------ after merge sym partition ---------------------")
  (pretty-display sol-map)
  sol-map)

(define heuristic-partitioner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [space (make-hash)])

    (define network (list))

    (define (inc-space place sp)
      (pretty-display `(inc-space ,place))
      (if (is-a? place TypeExpansion%)
          (for ([p (get-field place-list place)])
               (inc-space p sp))
          (if (hash-has-key? space place)
              (hash-set! space place (+ (hash-ref space place) sp))
              (hash-set! space place sp))))
    
    (define (add-network p1 p2)
      (cond
       [(and (is-a? p1 TypeExpansion%) (is-a? p2 TypeExpansion%))
        (for ([x (get-field place-list p1)]
              [y (get-field place-list p2)])
             (add-network x y))]

       [(is-a? p1 TypeExpansion%)
        (add-network (car (get-field place-list p1)) p2)]

       [(is-a? p2 TypeExpansion%)
        (add-network (car (get-field place-list p2)) p1)]

       [(and (not (equal? p1 p2)) 
             ;; (not (equal? p1 (* 2 w)))
             ;; (not (equal? p2 (* 2 w)))
             (not (is-a? p1 Place%))
             (not (is-a? p2 Place%))
             (or (symbolic? p1) (symbolic? p2)))
        (set! network (cons (cons p1 p2) network))]))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (inc-space (get-field place-type ast) est-num)
        (set (get-field place-type ast))
        ]

       [(is-a? ast Array%)
        (define index (get-field index ast))
        (define place-type (get-field place-type ast))
        (inc-space place-type est-acc-arr)

        ;; Infer place
        (send index infer-place place-type)
        (define index-ret (send index accept this))
        
        (add-network place-type (get-field place-type index))
        (set-add index-ret place-type)
        ]

       [(is-a? ast Var%)
        (inc-space (get-field place-type ast) est-var)
        (set (get-field place-type ast))
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        
        ;; set place-type
        (define place-type (get-field place-type ast))
        
        ;; Infer place
        (send e1 infer-place place-type)
        (define e1-ret (send e1 accept this))
        
        (inc-space place-type (hash-ref space-map (get-field op op)))
        (add-network place-type (get-field place-type e1))
        (set-add e1-ret place-type)
        ]

       [(is-a? ast BinExp%)
        (pretty-display (format "HEU: BinExp% ~a, place-type = ~a" 
                                (send ast to-string) (get-field place-type ast)))
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
        (add-network place-type (get-field place-type e1))
        (add-network place-type (get-field place-type e2))
        (set-union e1-ret e2-ret (set place-type))
        ]

       [(is-a? ast FuncCall%)
        ;; infer place-type
        (for ([param (get-field stmts (get-field args (get-field signature ast)))]
              [arg (flatten-arg (get-field args ast))])
             (send arg infer-place (get-field place-type param))
             (add-network (get-field place-type arg) (get-field place-type param))
             )
        
        ;; visit children
        (define args-ret (set))
        (for ([arg (get-field args ast)])
             (set! args-ret (set-union args-ret (send arg accept this))))
        args-ret
        ]

       [(is-a? ast Assign%)
        (pretty-display (format "HEU: Assign% ~a = ~a" 
                                (send (get-field lhs ast) to-string)
                                (send (get-field rhs ast) to-string)))
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))

        ;; infer place
        (send rhs infer-place (get-field place-type lhs))
        (send lhs infer-place (get-field place-type rhs))

        (define rhs-ret (send rhs accept this))
        (define lhs-ret (send lhs accept this))

        (add-network (get-field place-type lhs) (get-field place-type rhs))
        (set-union rhs-ret lhs-ret)
        ]

       [(is-a? ast VarDecl%)
        (define place (get-field place ast))
        (inc-space place (* (length (get-field var-list ast))
                            (if (is-a? ast Param%)
                                (add1 est-data)
                                est-data)))
        (set place)
        ]

       [(is-a? ast ArrayDecl%)
        (define place-list (get-field place-list ast))
        (define last 0)
        (define place-set
          (for/set ([p place-list])
            (let* ([from (get-field from p)]
                   [to   (get-field to p)])
              (when (not (= from last))
                    (send ast bound-error))
              (set! last to)
              (inc-space (get-field place p) (* (- to from) est-data)) ; increase space
              (get-field place p)
              )))

        (when (not (= (get-field bound ast) last))
              (send ast bound-error))
        place-set
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

        (send (get-field body ast) accept this)
        ]

       [(is-a? ast If%)
        (define cond-ret (send (get-field condition ast) accept this))

        (define body-ret (send (get-field true-block ast) accept this))
        (when (get-field false-block ast)
            (set! body-ret 
                  (set-union body-ret (send (get-field false-block ast) accept this))))
        (for* ([a cond-ret]
               [b body-ret])
              (add-network a b))
        (set-union cond-ret body-ret)
        ]

       [(is-a? ast While%)
        (define pre-ret (send (get-field pre ast) accept this))
        (define cond-ret (send (get-field condition ast) accept this))
        (define body-ret (send (get-field body ast) accept this))

        (for* ([a cond-ret]
               [b (set-union body-ret pre-ret)])
              (add-network a b))
        (set-union pre-ret cond-ret body-ret)
        ]

       [(is-a? ast Return%) (set)]
       
       [(is-a? ast FuncDecl%)
        (when (get-field return ast)
              (send (get-field return ast) accept this))
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Program%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))

        (pretty-display `(network1 ,network))
        (values space network)]

       [(is-a? ast Block%)
        (foldl (lambda (stmt all) (set-union all (send stmt accept this)))
               (set) (get-field stmts ast))]

       [else
        (raise (format "visitor-heupartition: unimplemented for ~a" ast))]
       ))))

      