#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" 
         "visitor-interface.rkt" 
         "visitor-expr-interpreter.rkt")

;;(require rosette/solver/kodkod/kodkod)
;;(require rosette/solver/smt/z3)

(provide (all-defined-out))

(define (get-place place-type var from to)
  ;; (pretty-display (format "GET-PLACE: ~a ~a [~a , ~a]" 
  ;;                         (place-to-string place-type)
  ;;                         var from to))

  (define (get-place-at dist index)
    (if (empty? dist)
        #f
        (let ([from (get-field from (car dist))]
              [to   (get-field to   (car dist))])
          ;;(when debug (pretty-display `(get-place-at ,(place-to-string dist) ,index
          ;;                                           ,from ,to)))
          (if (and (>= index from) (< index to))
              (car dist)
              (get-place-at (cdr dist) index)))))
  
  (define (inner)
    (define dist (car place-type))
    (define index (cdr place-type))
    (define result (send index accept (new expr-interpreter% 
                                           [var var] 
                                           [from from] [to to])))
    
    ;; (pretty-display "RESULT:")
    ;; (send result pretty-print)
    (if (is-a? result Range%)
        (let ([p1 (get-place-at dist (get-field from result))]
              [p2 (get-place-at dist (get-field to result))])
          (assert (equal? p1 p2) "not same place")
          p1)
        (cons dist result)))
  
  (and (<= from to) (place-type-dist? place-type)
       (inner)))
  

;; Unroll for loop according to array distributions of variables inside its body.
;; The sub AST inside unrolled for loop is duplicated.
;; This visitor mutates the AST.
(define loopbound-computer%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [stack (list)] [current-list (list)] [all-ranges (list)] [for-list (list)])
    (define debug #f)

    (define (get-sym-bound)
      (define-symbolic* bound integer?)
      bound)

    (define (construct-ranges from to n)
      ;; (list (cons from 4)
      ;;       (cons 5 6)
      ;;       (cons 7 7)
      ;;       (cons 8 (sub1 to)))

      (define (assert-order lst)
        (when (>= (length lst) 3)
              (assert (and (<= (first lst) (second lst))
                           (<= (second lst) (third lst))))
              (assert-order (cdr lst))))

      (define (construct-pairs lst)
        (if (>= (length lst) 2)
            (cons (cons (first lst) (sub1 (second lst)))
                  (construct-pairs (cdr lst)))
            (list)))

      (define bounds (append (list from) 
                             (for/list ([i (in-range (sub1 n))]) (get-sym-bound))
                             (list to)))

      (when debug (pretty-display `(bound-array ,bounds)))
      (assert-order bounds)
      (construct-pairs bounds)
      )
    
    (define (add-to-set dist-place)
      (set! current-list (cons dist-place current-list)))

    (define/public (visit ast)
      (define (check)
        (let ([place-type (get-field place-type ast)])
          (when debug (pretty-display `(check ,(send ast to-string) ,place-type)))
          (when (place-type-dist? place-type)
            (add-to-set place-type))))

      (cond
       [(is-a? ast Array%)
        ;(when debug (pretty-display (format "UNROLL: Array ~a" (send ast to-string))))
        (send (get-field index ast) accept this)
        (check)]
       
       [(or (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Param%))
        ;(when debug (pretty-display (format "UNROLL: ~a" (send ast to-string))))
        (check)]

       [(is-a? ast UnaExp%)
        (when debug (pretty-display (format "LOOPBOUND: UnaExp ~a" (send ast to-string))))
        (send (get-field e1 ast) accept this)
        (check)]
       
       [(is-a? ast BinExp%)
        ;(when debug (pretty-display (format "UNROLL: BinExp ~a" (send ast to-string))))
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (check)]

       [(is-a? ast FuncCall%)
        ;(when debug (pretty-display (format "UNROLL: FuncCall ~a" (send ast to-string))))
        ;(send (get-field signature ast) accept this)
        (for ([arg (get-field args ast)])
             (send arg accept this))
        (check)]

       [(is-a? ast For%)
        ;; Do actual stuff here
        (define body (get-field body ast))
        (define iter (get-field iter ast))
        (define from (get-field from ast))
        (define to (get-field to ast))
        (when debug (pretty-display (format "LOOPBOUND: For ~a (1)" (send iter to-string))))

        ;; new scope
        (set! stack (cons current-list stack))
        (set! current-list (list))

        ;; body
        (send body accept this)

        (when debug (pretty-display (format "LOOPBOUND: For ~a (2)" (send iter to-string))))
        (define ranges (construct-ranges from to max-unroll))
        (when debug (pretty-display `(ranges ,ranges)))
        (define new-list
          (for*/list ([range ranges]
                      [place-type current-list])
                     (get-place place-type (get-field name iter) (car range) (cdr range))))


        (solve (assert #t))
        #;(when debug
              (pretty-display (current-solution)))
             
        ;; merge scope
        (set! current-list (append (car stack) new-list))
        (set! stack (cdr stack))

        (set-field! unroll ast ranges)
        (set! all-ranges (cons ranges all-ranges))
        (set! for-list (cons ast for-list))
        ]

       [(is-a? ast If%)
        ;(pretty-display "UNROLL: If")
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Assign%)
        ;(pretty-display "UNROLL: Assign")
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)]

       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            (for ([v val])
                 (send v accept this))
            (send val accept this))]

       [(is-a? ast Program%)
                  
	;;(current-solver (new kodkod-incremental%))
	(current-bitwidth 32)

        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))
        (define n-ranges 0)
        (for* ([ranges all-ranges]
               [range ranges])
              (set! n-ranges (+ n-ranges 
                                (if (> (car range) (cdr range)) 0 1))))

        (define my-sol (solve (assert #t)))
        
        (when debug
              (pretty-display `(all-ranges ,(evaluate all-ranges my-sol)))
              (pretty-display `(n-ranges ,(evaluate n-ranges my-sol))))


        (define (loop)
          (define sol (solve (assert (< n-ranges (evaluate n-ranges my-sol)))))
          (when debug
                (pretty-display `(all-ranges ,(evaluate all-ranges my-sol)))
                (pretty-display `(n-ranges ,(evaluate n-ranges my-sol))))
          (when (sat? sol)
            (set! my-sol sol)
            (loop)))
        
	(define t (current-seconds))
	(loop)

        (define (evaluate-unroll)
          (when debug
            (pretty-display "FINAL")
            (pretty-display `(all-ranges ,(evaluate all-ranges my-sol)))
            (pretty-display `(n-ranges ,(evaluate n-ranges my-sol))))
          (for ([x for-list])
            (let* ([ranges (evaluate (get-field unroll x) my-sol)]
                   [filtered (filter (lambda (x) (<= (car x) (cdr x))) ranges)])
              (send x set-unroll filtered))))
        (evaluate-unroll)
        
        (define solver (current-solver))
	(solver-clear solver)
	;;(current-solution (empty-solution))
	(pretty-display (format "Loopbound Synthesis time = ~a" (- (current-seconds) t)))
        ]

       [(is-a? ast Block%)
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))
        ]

       [(is-a? ast FuncDecl%)
        (send (get-field body ast) accept this)]

       ))))
       
            
