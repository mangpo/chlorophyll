#lang s-exp rosette

(require "ast.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define partition-to-number%
  (class* object% (visitor<%>)
    (super-new)
    (init-field num-core 
                real-place-set
                [num-to-part (make-vector num-core #f)]
                [part-to-num (make-hash)]
                [next-core 0])

    (set-for-each real-place-set
                  (lambda(p) 
                    (when (number? p)
                          (vector-set! num-to-part p p)
                          (hash-set! part-to-num p p))))
    

    (define (get-next-core)
      (set! next-core (add1 next-core))
      (if (vector-ref num-to-part next-core)
          (get-next-core)
          next-core))
    
    (define (convert-to-num p)
      (cond
        #|[(equal? p "??")
         (let ([num (send ast (get-sym))])
           (hash-set! part-to-num p num)
           num)]|#
        [(hash-has-key? part-to-num p)
         (hash-ref part-to-num p)]
        [else
         (let ([num (get-next-core)])
           (hash-set! part-to-num p num)
           (vector-set! num-to-part num p)
           num)]))

    (define/public (visit ast)
      (define (check-place)
        (let ([p (get-field place ast)])
          (when (string? p)
            (set-field! place ast (convert-to-num p)))))

      (cond
       [(or (or (is-a? ast Num%) (is-a? ast Op%)) (is-a? ast VarDecl%))
        (check-place)]
       
       [(is-a? ast Var%)
        (void)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send e2 accept this)
        (send op accept this)
        (check-place)]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send op accept this)
        (check-place)]

       [(is-a? ast Assign%) 
        (define rhs (get-field rhs ast))
        (send rhs accept this)]

       [(is-a? ast Block%) 
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [else (raise "Error: partition-to-number visitor unimplemented!")]
       ))
    ))
            
        
                