#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "arrayforth.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

;; Evaluate expression by treating constants as they are
;; but treating variables as 0s.
(define lowerbound%
  (class* object% (visitor<%>)
    (super-new)

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        (get-field n (get-field n ast))]

       [(is-a? ast Var%)
        0]

       [(is-a? ast UnaExp%)
        (define op (get-field op (get-field op ast)))
        (define e1 (send (get-field e1 ast) accept this))

        (cond
         [(equal? op "~")
          (bitwise-xor e1 (sub1 (arithmetic-shift 1 ga-bit)))]
         [(equal? op "-")
          (- 0 e1)]
         [else
          (raise (format "visitor-lowerbound: unimplemented for unary operator ~a" op))])]

       [(is-a? ast BinExp%)
        (define op (get-field op (get-field op ast)))
        (define e1 (send (get-field e1 ast) accept this))
        (define e2 (send (get-field e2 ast) accept this))
        
        (cond
         [(equal? op "+")  (+ e1 e2)]
         [(equal? op "-")  (- e1 e2)]
         [(equal? op "*")  (* e1 e2)]
         [(equal? op ">>") (arithmetic-shift e1 (- 0 e2))]
         [(equal? op "<<") (arithmetic-shift e1 e2)]
         [(equal? op "&")  (bitwise-and e1 e2)]
         [(equal? op "|")  (bitwise-ior e1 e2)]
         [(equal? op "^")  (bitwise-xor e1 e2)]
         [else 
          (raise (format "visitor-lowerbound: unimplemented for binary operator ~a" op))])]

       [(is-a? ast FuncCall%)
        0]

       [else
        (raise (format "visitor-lowerbound: unimplemented for ~a" ast))]))))
        

    