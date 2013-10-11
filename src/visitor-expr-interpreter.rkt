#lang s-exp rosette

(require "header.rkt" "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define expr-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field var from to)

    (define (compute-range e1 e2 op)
      (cons (op (car e1) (car e2)) (op (cdr e1) (cdr e2))))

    (define/public (visit ast)
      (cond
       [(is-a? ast Array%)
        (raise (format "visitor-expr-interpreter: not support Array '~a' at line ~a"
                       (get-field name ast) (send ast get-line)))]
       
       [(is-a? ast Num%)
        (define n (get-field n (get-field n ast)))
        (new Range% [from n] [to n])]

       [(is-a? ast Var%)
        (if (equal? (get-field name ast) var)
            (new Range% [from from] [to to])
            (send ast clone))]

       [(is-a? ast UnaExp%)
        (define e1-ret (send (get-field e1 ast) accept this))
        (define op (get-field op (get-field op ast)))
        (if (is-a? e1-ret Range%)
            (let ([e1-from (get-field from e1-ret)]
                  [e1-to (get-field to e1-ret)])
              (if (equal? op "-")
                  (new Range% 
                       [from (- e1-to)]
                       [to (- e1-from)])
                  (raise (format "visitor-exp-interpreter: not support Unaray operation '~a' at line ~a" op (send ast get-line)))))
            (send ast clone))]

       [(is-a? ast BinExp%)
        ;(pretty-display (format "EXPR: BinExp ~a" (send ast to-string)))
        (define e1-ret (send (get-field e1 ast) accept this))
        (define e2-ret (send (get-field e2 ast) accept this))
        (define op (get-field op (get-field op ast)))

        (if (and (is-a? e1-ret Range%) (is-a? e2-ret Range%))
            (let* ([e1-from (get-field from e1-ret)]
                   [e1-to (get-field to e1-ret)]
                   [e2-from (get-field from e2-ret)]
                   [e2-to (get-field to e2-ret)]
                   [range 
                    (lambda (op) 
                      (new Range% [from (op e1-from e2-from)] [to (op e1-to e2-to)]))])
              ;(pretty-display `(ret ,e1-from ,e1-to ,e2-from ,e2-to))
              ;(pretty-display `(e1-to ,e1-to ,(number? e1-to) ,(symbolic? e1-to) ,(list? e1-to)))
              (cond
               [(equal? op "*") (range *)]
               
               [(equal? op "+") (range +)]
               
               [(equal? op "<<") (range arithmetic-shift)]
               
               [(equal? op "-")
                (new Range% [from (- e1-from e2-to)] [to (- e1-to e2-from)])]
               
               [(equal? op ">>")
                (new Range% 
                     [from (arithmetic-shift e1-from (- e2-to))] 
                     [to (arithmetic-shift e1-to (- e2-from))])]
               
               [else
                (raise (format "visitor-expr-interpreter: not support Binary operation '~a' at line ~a" op (send ast get-line)))]))

            (new BinExp% [op (get-field op ast)] [e1 e1-ret] [e2 e2-ret]))]

       [(is-a? ast Range%)
        ast]

       [else
        (raise (format "visitor-expr-interpreter: unimplemented for ~a" ast))]

       ))))
             
        