#lang racket

(require "ast.rkt" "parser.rkt" "visitor-interface.rkt")

;(define test "known int x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@10 -1@10 *@10 2@10;")
;(define test "known int x; x = (-1@1 &@1 100@1) <@4 (!@5 2@5 ||@5 20@5) +@18 -1@10 *@10 2@10;")
(define test "known int x; x = 1@1 +@10 -1@10;")

(define my-ast (ast-from-string test))

(send my-ast pretty-print)

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env #hash()])

    (define (count-msg x y)
      (if (equal? x y) 0 1))

    (define (get-count l)
      (car l))

    (define (get-place l)
      (car (cdr l)))
    
    (define (get-known l)
      (car (cdr (cdr l))))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
          (list 0 (get-field place ast) #t)]

       [(is-a? ast Var%) ; multiple places?
          (cons 0 (dict-ref env (get-field name ast)))]

       [(is-a? ast UnaExp%)
          (define e1-ret (send (get-field e1 ast) accept this))
          (define op-place (get-field place (get-field op ast)))
          
          (list (+ (get-count e1-ret) (count-msg op-place (get-place e1-ret)))
                op-place
                (get-known e1-ret))]

       [(is-a? ast BinExp%)
          (define e1-ret (send (get-field e1 ast) accept this))
          (define e2-ret (send (get-field e2 ast) accept this))
          (define op-place (get-field place (get-field op ast)))

          (list (+ (+ (+ (get-count e1-ret) (get-count e2-ret))
                      (count-msg op-place (get-place e1-ret)))
                   (count-msg op-place (get-place e2-ret)))
                op-place
                (and (get-known e1-ret) (get-known e2-ret)))]
                
       [(is-a? ast VarDecl%) 
          (set! env (dict-set env (get-field var ast) (send ast get-place-known)))
          0]

       [(is-a? ast Assign%) 
          (define lhs-place (cdr (dict-ref env (get-field lhs ast))))
          (define rhs-ret (send (get-field rhs ast) accept this))
       
          (+ (get-count rhs-ret) (count-msg lhs-place (get-place rhs-ret)))
        ]

       [(is-a? ast Block%) 
          (foldl (lambda (stmt sum) (+ sum (send stmt accept this))) 
                 0 
                 (get-field stmts ast))]
       [else 100]))
))

(define interpreter (new count-msg-interpreter%))
(pretty-display (send my-ast accept interpreter))