#lang racket

(require "ast.rkt" "parser.rkt" "visitor-interface.rkt" "space-estimator.rkt")

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [places (make-hash)] [capacity 256])

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place [add-space 1])
      (define space (if (dict-has-key? places place)
                        (dict-ref places place)
                        0))
      (set! space (+ space add-space))
      (dict-set! places place space)
      (when (> space capacity)
            (raise (format "Error: exceed capacity of core ~a" place))))

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg x y)
      (cond 
        [(equal? x y) 0]
        [(equal? x "any") 0]
        [(equal? y "any") 0]
        [else 
         (inc-space x est-comm)
         (inc-space y est-comm)
         1]))

    (define/public (display-used-space)
      (pretty-display places))
        
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
          (inc-space (get-field place ast) est-num)
          0]

       [(is-a? ast Var%) ; multiple places?
          (define place-known (dict-ref env (get-field name ast) 
                                        (lambda () (send ast not-found-error))))
          (send ast set-place-known place-known)
          (inc-space (get-field place ast) est-var)
          0]

       [(is-a? ast UnaExp%)
          (define e1 (get-field e1 ast))
          (define e1-count (send e1 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (get-field known-type e1))
          (inc-space op-place (est-space (get-field op (get-field op ast)))) ; increase space

          (+ e1-count (count-msg op-place (get-field place e1)))]

       [(is-a? ast BinExp%)
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define e1-count (send e1 accept this))
          (define e2-count (send e2 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (and (get-field known-type e1) (get-field known-type e2)))
          (inc-space op-place (est-space (get-field op (get-field op ast)))) ; increase space
          
          (+ (+ (+ e1-count e2-count)
                      (count-msg op-place (get-field place e1)))
                   (count-msg op-place (get-field place e2)))]
                
       [(is-a? ast VarDecl%) 
          (dict-set! env (get-field var ast) (send ast get-place-known))
          (inc-space (get-field place ast) est-data) ; include space
          0]

       [(is-a? ast Assign%) 
          (define lhs (get-field lhs ast))
          (define rhs (get-field rhs ast))

          ;;; Visit lhs
          (send lhs accept this)

          (define lhs-place (get-field place lhs))
          (define lhs-known (get-field known-type lhs))

          ;;; If rhs is a number, set place to be equal to lhs
          (when (is-a? rhs Num%) (set-field! place rhs lhs-place))

          ;;; Visit rhs
          (define rhs-count (send rhs accept this))

          ;;; Update dynamic known type
          (define rhs-known (get-field place rhs))
          (when (and (not rhs-known) lhs-known)
                (set-field! known-type lhs #f)
                (dict-set! env (get-field name lhs) (send lhs get-place-known)))
       
          (+ rhs-count (count-msg lhs-place (get-field place rhs)))
        ]

       [(is-a? ast Block%) 
          (foldl (lambda (stmt sum) (+ sum (send stmt accept this))) 
                 0 
                 (get-field stmts ast))]
       [else (raise "Error: count-msg-interpreter unimplemented!")]))
))

;(define test "known int@4 x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")
(define test "int@4 x; x = (-1 &@1 x) <@4 (!@5 2 ||@5 20) +@10 -1 *@10 2; x = 1;")
;(define my-ast (ast-from-string test))
(define my-ast (ast-from-file "program.mylang"))

(send my-ast pretty-print)

(define interpreter (new count-msg-interpreter%))
(pretty-display (send my-ast accept interpreter))

(send my-ast pretty-print)

(send interpreter display-used-space)