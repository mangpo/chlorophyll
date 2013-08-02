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
      (define (check-base p)
        (cond 
         [(string? p)
          (convert-to-num p)]
         [(list? p)
          (for ([i p])
               (send i accept this))
          p]
         [(pair? p)
          (for ([i (car p)])
               (send i accept this))
          p]

         [(is-a? p TypeExpansion%)
          (set-field! place-list p
                      (map (lambda (x) (check-base x)) (get-field place-list p)))
          p]

         [else p]))

      (define (check-place)
        (set-field! place ast (check-base (get-field place ast))))

      (define (check-place-type)
        (set-field! place-type ast (check-base (get-field place-type ast))))

      (cond
       [(is-a? ast Livable%)
        (check-place)]

       [(is-a? ast LivableGroup%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (string? place)
                    (set-field! place-list ast (convert-to-num place)))))]

       [(is-a? ast For%)
        (let ([place (get-field place-list ast)])
          (if (list? place)
              (for ([p (get-field place-list ast)])
                   (send p accept this))
              (when (string? place)
                    (set-field! place-list ast (convert-to-num place)))))
	(send (get-field body ast) accept this)]
       
       [(or (is-a? ast Var%) (is-a? ast Num%))
        (void)]

       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send e2 accept this)
        (send op accept this)
	(check-place-type)
        ]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        (send e1 accept this)
        (send op accept this)
	(check-place-type)]
       
       [(is-a? ast FuncCall%)
	(for ([arg (get-field args ast)])
	     (send arg accept this))]

       [(is-a? ast Assign%) 
	(send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)]

       [(is-a? ast Return%) 
	void]

       [(is-a? ast If%)
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))]

       [(is-a? ast While%)
	(send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)]

       [(is-a? ast Forever%)
        (send (get-field body ast) accept this)]

       [(is-a? ast Block%) 
        (for ([stmt (get-field stmts ast)])
             (send stmt accept this))]

       [(is-a? ast FuncDecl%)
        (send (get-field return ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)]
       
       [(is-a? ast ConcreteFilterDecl%)
        (send (get-field input ast) accept this)
        (send (get-field output ast) accept this)
        (send (get-field args ast) accept this)
        (send (get-field body ast) accept this)]

       [else (raise (format "visitor-rename: unimplemented for ~a" ast))]
       ))))
