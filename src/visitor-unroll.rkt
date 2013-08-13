#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-cloner.rkt")

(provide (all-defined-out))

;; Unroll for loop according to array distributions of variables inside its body.
;; The sub AST inside unrolled for loop is duplicated.
;; This visitor mutates the AST.
(define loop-unroller%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [index-map (make-hash)] [cloner (new range-cloner%)])

    ;; We don't care about place here. We just need ranges.dict
    (define (construct-placelist x-placelist y-placelist index)
      (cond
        [(and (empty? x-placelist) (empty? y-placelist))
         (list)]
         
        [(empty? x-placelist)
         (set-field! from (car y-placelist) index)
         y-placelist]
        
        [(empty? y-placelist)
         (set-field! from (car x-placelist) index)
         x-placelist]
        
        [else
         (let* ([x-first (car x-placelist)]
                [y-first (car y-placelist)]
                [x-to (get-field to x-first)]
                [y-to (get-field to y-first)])
           (cond 
             
             [(equal? x-to y-to)
              (cons (new RangePlace% [from index] [to x-to] 
                         [place #f])
                    (construct-placelist (cdr x-placelist) (cdr y-placelist) x-to))]
             
             [(< x-to y-to)
              (cons (new RangePlace% [from index] [to x-to]
                         [place #f])
                    (construct-placelist (cdr x-placelist) y-placelist x-to))]
             
             [(> x-to y-to)
              (cons (new RangePlace% [from index] [to y-to]
                         [place #f])
                    (construct-placelist x-placelist (cdr y-placelist) y-to))]
             
             [else (raise "contruct-placelist: unimplemented")]))]))
    
    (define (push-scope)
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" index-map)
        (set! index-map new-env)))

    (define (pop-scope)
      (set! index-map (dict-ref index-map "__up__")))

    (define (lookup ast)
      (define (lookup-name env name)
        ;(pretty-display `(lookup-name ,env ,name))
        (dict-ref env name
                  (lambda () (lookup-name (dict-ref env "__up__"
                                                    (lambda () #f))
                                          name))))

      (if (is-a? ast Var%)
          (lookup-name index-map (get-field name ast))
          #f))
    
    (define (update ast ranges)
      (define (update-name env name)
        (if (dict-has-key? env name)
            (dict-set! env name (construct-placelist (dict-ref env name) ranges 0))
            (update-name (dict-ref env "__up__") name)))

      (update-name index-map (get-field name ast)))

    (define/public (visit ast)
      (define (check)
        ;(pretty-display `(check ,(send ast to-string)))
        (let ([place-type (get-field place-type ast)])
          (when (and (place-type-dist? place-type)
                     (lookup (cdr place-type)))
            (update (cdr place-type) (car place-type)))))

      (cond
       [(or (is-a? ast Num%)
            (is-a? ast Var%)
            (is-a? ast Param%))
        (check)
        ast]

       [(is-a? ast UnaExp%)
        (send (get-field e1 ast) accept this)
        (check)
        ast]
       
       [(is-a? ast BinExp%)
        (send (get-field e1 ast) accept this)
        (send (get-field e2 ast) accept this)
        (check)
        ast]

       [(is-a? ast FuncCall%)
        (send (get-field signature ast) accept this)
        (for ([arg (get-field args ast)])
             (send arg accept this))
        (check)
        ast]

       [(is-a? ast VarDecl%) 
        ;(pretty-display "UNROLL: VarDecl (before declare)")
        (for ([var (get-field var-list ast)])
             ;; Delcare as #f because this is not loop index.
             (declare index-map var #f))
        ;(pretty-display "UNROLL: VarDecl (after declare)")
        ast]

       [(is-a? ast ArrayDecl%)
        ;(pretty-display "UNROLL: VarDecl (before declare)")
        (declare index-map (get-field var ast) #f)
        ;(pretty-display "UNROLL: VarDecl (after declare)")
        ast]

       [(is-a? ast For%)
        ;; Do actual stuff here
        (define body (get-field body ast))
        (define iter (get-field iter ast))
        (define iter-name (get-field name iter))

        ;(pretty-display "UNROLL: For (before visit body)")
        (push-scope)
        (declare index-map iter-name (list))
        (send body accept this)
        
        ;(pretty-display "UNROLL: For (after visit body)")
        
        (define (adjust range-list from to)
          ;; Filter out the ranges that fall outside region of interest.
          (let ([filtered (filter (lambda (x) (and (> (get-field to x) from) (< (get-field from x) to))) 
                                  range-list)])
            (unless (empty? filtered)
              (set-field! from (car filtered) from)
              (set-field! to (last filtered) to))
            filtered))

        (let ([rangeplace-list (adjust (lookup iter) (get-field from ast) (get-field to ast))])
          ;(undeclare index-map iter-name)
          (pop-scope)
          ;(pretty-display "UNROLL: For (after lookup)")
          ;(pretty-display `(rangeplace-list ,rangeplace-list))
          (if (empty? rangeplace-list)
              ast
              (for/list ([rangeplace rangeplace-list])
                        (send cloner set-range rangeplace iter-name)
                        (new For% 
                             [iter (send iter clone)] 
                             [body (send body accept cloner)]
                             [from (get-field from rangeplace)]
                             [to (get-field to rangeplace)]
                             [known (get-field known ast)]
                             [place-list (get-field place rangeplace)]
                             [body-placeset (get-field body-placeset ast)]))))
        ;; Return list of For%
        ]

       [(is-a? ast If%)
        ;(pretty-display "UNROLL: If")
        (send (get-field condition ast) accept this)
        (send (get-field true-block ast) accept this)
        (when (get-field false-block ast)
              (send (get-field false-block ast) accept this))
        ast]

       [(is-a? ast While%)
        (send (get-field pre ast) accept this)
        (send (get-field condition ast) accept this)
        (send (get-field body ast) accept this)
        ast]

       [(is-a? ast Assign%)
        ;(pretty-display "UNROLL: Assign")
        (send (get-field lhs ast) accept this)
        (send (get-field rhs ast) accept this)
        ast]

       [(is-a? ast Return%)
        ast]

       [(is-a? ast Program%)
        (for ([decl (get-field stmts ast)])
             (send decl accept this))]

       [(is-a? ast Block%)
        (set-field! stmts ast
                    (flatten (map (lambda (x) (send x accept this)) 
                                  (get-field stmts ast))))
        ast
        ]

       [(is-a? ast FuncDecl%)
        (for ([arg (get-field stmts (get-field args ast))])
             (send arg accept this))
        (send (get-field body ast) accept this)]

       ))))
       
            
