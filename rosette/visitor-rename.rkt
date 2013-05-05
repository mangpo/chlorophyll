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
       [(is-a? ast Livable%)
        (check-place)]

       [(is-a? ast LivableGroup%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (string? place)
                    (set-field! place-list ast (convert-to-num place)))))
	(when (is-a? ast For%) (send (get-field body ast) accept this))]
       
       [(or (is-a? ast Var%) (is-a? ast Num%))
        (void)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send e2 accept this)
        (send op accept this)
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send op accept this)]

       [(is-a? ast Assign%) 
        (send (get-field rhs ast) accept this)]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Block%) 
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (send (get-field return ast) accept this)
        (for ([stmt (get-field args ast)])
             (send stmt accept this))
        (send (get-field body ast) accept this)]

       [else (raise (format "Error: in partition-to-number, ~a unimplemented!" ast))]
       ))
    ))
            
        
                
