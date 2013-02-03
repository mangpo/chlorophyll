#lang racket

(require "ast.rkt" "parser.rkt" "visitor-interface.rkt")

;(define test "known int x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")
(define test "known int@4 x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")

(define my-ast (ast-from-string test))

(send my-ast pretty-print)

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env #hash()])

    (define (count-msg x y)
      (cond 
        [(equal? x y) 0]
        [(equal? x "any") 0]
        [(equal? y "any") 0]
        [else 1]))
        
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
          0]

       [(is-a? ast Var%) ; multiple places?
          0]

       [(is-a? ast UnaExp%)
          (define e1 (get-field e1 ast))
          (define e1-count (send e1 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (get-field known-type e1))
          
          (+ e1-count (count-msg op-place (get-field place e1)))]

       [(is-a? ast BinExp%)
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define e1-count (send e1 accept this))
          (define e2-count (send e2 accept this))
          (define op-place (get-field place ast))
          (set-field! known-type ast (and (get-field known-type e1) (get-field known-type e2)))
          
          (+ (+ (+ e1-count e2-count)
                      (count-msg op-place (get-field place e1)))
                   (count-msg op-place (get-field place e2)))]
                
       [(is-a? ast VarDecl%) 
          (set! env (dict-set env (get-field var ast) (send ast get-place-known)))
          0]

       [(is-a? ast Assign%) 
          (define rhs (get-field rhs ast))
          (define lhs-place (car (dict-ref env (get-field lhs ast))))
          (define rhs-count (send rhs accept this))
       
          (+ rhs-count (count-msg lhs-place (get-field place rhs)))
        ]

       [(is-a? ast Block%) 
          (foldl (lambda (stmt sum) (+ sum (send stmt accept this))) 
                 0 
                 (get-field stmts ast))]
       [else (raise "count-msg-interpreter: unimplemented!")]))
))

(define interpreter (new count-msg-interpreter%))
(pretty-display (send my-ast accept interpreter))