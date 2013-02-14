#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interface.rkt" "space-estimator.rkt" 
         "symbolic-dict.rkt")

(define debug #t)


(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [places (make-cores #:capacity 256 #:max-cores 144)])

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      (cores-inc-space places place add-space))
    
    (define (inc-space-with-op place op)
      (cores-add-op places place op))
    ;; (define (inc-space place add-space)
    ;;   (when (not (dict-has-key? places place))
    ;;         (dict-set! places place (core 0 (set))))
    ;;   (define core-info (dict-ref places place))
    ;;   (define space (+ (core-space core-info) add-space))
    ;;   (set-core-space! core-info space)
      
    ;;   (when debug
    ;;         (pretty-display (format "ADD-SPACE: place = ~a, add = ~a, current =~a" 
    ;;                                 place add-space space)))
    ;;   (when (> space capacity)
    ;;         (raise (format "Error: exceed capacity of core ~a" place))))
 
      
    ;; (define (inc-space-with-op place op)
    ;;   (define core-info (if (dict-has-key? places place)
    ;;                     (dict-ref places place)
    ;;                     (core 0 (set))))
    ;;   (define space (core-space core-info))
    ;;   (define costly-op (core-costly-op core-info))
    ;;   (define add-space (est-space op))

    ;;   (when (> add-space 4)
    ;;       (if (set-member? costly-op op)
    ;;           (set! add-space 4)
    ;;           (set! costly-op (set-add costly-op op))))

    ;;   (set! space (+ space add-space))
    ;;   (dict-set! places place (core space costly-op))

    ;;   (when debug
    ;;         (pretty-display (format ">> ADD-SPACE: op = ~a, place = ~a, add = ~a, current =~a" 
    ;;                                 op place add-space space)))

    ;;   (when (> space capacity)
    ;;         (raise (format "Error: exceed capacity of core ~a" place))))

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg x y)
      (cond 
        [(equal? x y) 0]
        [(equal? x 0) 0] ;any
        [(equal? y 0) 0] ;any
        [else 
         (when debug
               (pretty-display "2-COMM"))
         (inc-space x est-comm)
         (inc-space y est-comm)
         1]))

    ;(define/public (display-used-space)
    ;  (dict-for-each places (lambda (k v) 
    ;     (pretty-display (format "core = ~a, space = ~a, ops = ~a" k (core-space v) (core-costly-op v))))))
        
    (define/public (display-used-space)
      (display-cores places))
      
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
          (when debug
                (pretty-display (format "Num ~a" (get-field n ast))))
          (inc-space (get-field place ast) est-num)
          0]

       [(is-a? ast Var%) ; multiple places?
          (define place-known (dict-ref env (get-field name ast) 
                                        (lambda () (send ast not-found-error))))
          (send ast set-place-known place-known)

          (when debug
                (pretty-display (format "Var ~a" (get-field name ast))))
          (inc-space (get-field place ast) est-var)
          0]

       [(is-a? ast UnaExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e1-count (send e1 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (get-field known-type e1))

          (when debug
                (pretty-display (format "UnaOp ~a" (get-field op (get-field op ast)))))
          (inc-space-with-op op-place (get-field op (get-field op ast))) ; increase space
          
          (+ e1-count (count-msg op-place (get-field place e1)))]

       [(is-a? ast BinExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define e1-count (send e1 accept this))
          (define e2-count (send e2 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (and (get-field known-type e1) (get-field known-type e2)))

          (when debug
                (pretty-display (format "BinOp ~a" (get-field op (get-field op ast)))))
          (inc-space-with-op op-place (get-field op (get-field op ast))) ; increase space

          (+ (+ (+ e1-count e2-count)
                      (count-msg op-place (get-field place e1)))
                   (count-msg op-place (get-field place e2)))]
                
       [(is-a? ast VarDecl%) 
          (define place-known (send ast get-place-known))
          (define place (get-field place ast))
          (define var-list (get-field var-list ast))
          (for ([var var-list])
               (dict-set! env var place-known))

          (when debug
                (pretty-display (format "VarDecl ~a" var-list)))
          (inc-space place (* (length var-list) est-data)) ; include space

          0]

       [(is-a? ast Assign%) 
          (when debug (newline))
          (define lhs (get-field lhs ast))
          (define rhs (get-field rhs ast))

          (when debug
                (pretty-display "Assign"))
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
(define my-ast (ast-from-file "examples/2.mylang"))

(send my-ast pretty-print)
(define interpreter (new count-msg-interpreter%))
(pretty-display (format "# messages = ~a" (send my-ast accept interpreter)))
(newline)
(send my-ast pretty-print)

(send interpreter display-used-space)
