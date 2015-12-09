#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide loop-optimizer%)

;; 1) Get rid of loop with 1 iteration
;; 2) Convert x[i+10] offset 10 to x[i]
(define loop-optimizer%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [iter-map (make-hash)])

    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" iter-map)
        (set! iter-map new-env))
      )

    (define (pop-scope)
      (set! iter-map (dict-ref iter-map "__up__")))

    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
        ast]
       
       [(is-a? ast Array%)
        (set-field! index ast (send (get-field index ast) accept this))
        (define index (get-field index ast))
        
        (when (is-a? index BinExp%)
          (define op (get-field op (get-field op index)))
          (define e1 (get-field e1 index))
          (define e2 (get-field e2 index))
          (define offset (get-field offset ast))
          (cond
           [(and (equal? op "+") (is-a? e1 Num%))
            (set-field! offset ast (- offset (get-field n (get-field n e1))))
            (set-field! index ast e2)]
                
           [(and (equal? op "+") (is-a? e2 Num%))
            (set-field! offset ast (- offset (get-field n (get-field n e2))))
            (set-field! index ast e1)]

           [(and (equal? op "-") (is-a? e2 Num%))
            (set-field! offset ast (+ offset (get-field n (get-field n e2))))
            (set-field! index ast e1)]
           ))
        ast
        ]

       [(is-a? ast Var%)
        (if (has-var? iter-map (get-field name ast))
            (let ([val (lookup iter-map ast)])
              (new Num% [n (new Const% [n val])] [type "int"]))
            ast)]

       [(is-a? ast UnaExp%)
        (set-field! e1 ast (send (get-field e1 ast) accept this))
        ast]

       [(is-a? ast BinExp%)
        (define e1 (send (get-field e1 ast) accept this))
        (define e2 (send (get-field e2 ast) accept this))
        (set-field! e1 ast e1)
        (set-field! e2 ast e2)

        (define op (send ast get-op))
        (when
         (member op (list "*" "/"))
         (define shf1 (and (is-a? e1 Num%) (log-2 (send e1 get-value))))
         (define shf2 (and (is-a? e2 Num%) (log-2 (send e2 get-value))))
         
         (when (and (equal? op "*") shf1)
               (define t e1)
               (set! e1 e2) (set! e2 t)
               (set! t shf1) (set! shf1 shf2) (set! shf2 t)
               )

         (when shf2
               (pretty-display "before >>")
               (send ast pretty-print)
               (define type (get-field type e1))
               (define shf
                 (if (fix_t? type)
                     (- shf2 (- 18 (fix_t-int type)))
                     shf2))
               (define new-op
                 (cond
                  [(equal? op "*") "<<"]
                  [(equal? op "/") ">>"]))

               (set-field! op ast (new Op% [op new-op]))
               (set-field! e1 ast e1)
               (set-field! e2 ast
                           (new Num% [n (new Const% [n shf])] [type "int"]))
               (pretty-display "after >>")
               (send ast pretty-print)
               ;;(raise "done")
               )
         )
        
        ast]

       [(is-a? ast FuncCall%)
        (set-field! args ast
                    (for/list ([arg (get-field args ast)])
                              (send arg accept this)))
        ast]

       [(or (is-a? ast Recv%)
            (is-a? ast PortListen%)
            (is-a? ast PortExec%))
        ast]

       [(is-a? ast Send%)
        (send (get-field data ast) accept this)
        ast]

       [(is-a? ast Assign%)
        (send (get-field lhs ast) accept this)
        (set-field! rhs ast (send (get-field rhs ast) accept this))
        ast]

       [(or (is-a? ast ArrayDecl%) (is-a? ast VarDecl%))
        ast]
       
       [(is-a? ast Return%)
        (define val (get-field val ast))
        (if (list? val)
            (set-field! val ast 
                        (for/list ([v val])
                          (send v accept this)))
            (set-field! val ast (send (get-field val ast) accept this)))
        ast]

       [(is-a? ast If%)
        (set-field! condition ast (send (get-field condition ast) accept this))
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
        ast]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (set-field! condition ast (send (get-field condition ast) accept this))
        (send (get-field body ast) accept this)
        ast]

       [(is-a? ast For%)
	(push-scope)

        (define ret
          (if (= (get-field from ast) (sub1 (get-field to ast)))
              ;; iterate only 1 time
              (begin
                (declare iter-map (get-field name (get-field iter ast)) 
                         (get-field from ast))
                (send (get-field body ast) accept this))
              (begin
                (send (get-field body ast) accept this)
                ast)))
        
        (pop-scope)
        ret]

       [(is-a? ast FuncDecl%)
        (send (get-field body ast) accept this)]

       [(is-a? ast Program%)
        (for ([decl (get-field stmts ast)])
	     (send decl accept this))]

       [(is-a? ast Block%)
        (set-field! stmts ast
                    (for/list ([stmt (get-field stmts ast)])
                              (send stmt accept this)))
        ast]
          
       [else
        (raise (format "visitor-loopopt: unimplemented for ~a" ast))]))))
          

(define (log-2 x)
  (define (loop n count)
    (if (= (modulo n 2) 0)
        (loop (arithmetic-shift n -1) (add1 count))
        (and (= n 1) count)))
  (and (> x 0) (loop (bitwise-and x #x3ffff) 0)))
    
