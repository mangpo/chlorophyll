#lang s-exp rosette

(require "ast.rkt" "parser.rkt" "visitor-interface.rkt" "space-estimator.rkt" 
         "symbolic-dict.rkt")

(provide (all-defined-out))

(define debug #f)

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] [places (make-cores #:capacity 256 #:max-cores 16)])

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      (cores-inc-space places place add-space))
    
    (define (inc-space-with-op place op)
      (cores-add-op places place op))

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

