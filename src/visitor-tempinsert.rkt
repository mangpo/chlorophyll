#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define temp-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [count 0] [new-decls (list)])

    (struct entry (temp type expand))

    (define (get-temp type expand expect place-type [funccall #f])
      (let ([temp (format "_temp~a" count)])
        (set! count (add1 count))
        (set! new-decls (cons
                         (if (and funccall (> expand 1))
                             ;; no expansion in desugar step
                             (new TempDecl% [var-list (list temp)]
                                  [type (cons type expand)] ; packed type
                                  [place place-type] 
                                  [expect expand]) 
                             (new TempDecl% [var-list (list temp)]
                                  [type type] ; native type
                                  [place place-type] 
                                  [expect expand]))
                              new-decls))
        ;(pretty-display `(declare ,temp ,expand ,expect))
        
        ;; temp for funccall:
        ;; let func() -> int::2
        ;; temp = func() 
        ;; type(temp) = (cons int 2)
        ;; expect(temp) = 1

        ;; don't set known-type
        (new Var% [name temp] [type type] [expand expand] [expect expect] [place-type place-type])
        ))

    (define/public (visit ast)
      (cond
        [(or (is-a? ast Num%)
             (is-a? ast Var%))
         (cons (list) ast)]
        
        [(is-a? ast UnaExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))
         
         ;; (define temp (get-temp (get-field type ast) 
         ;;                        (get-field expect ast)
         ;;                        (get-field expect ast)
         ;;                        (get-field place (get-field op ast))))
         ;; (cons (list (car e1-ret)
         ;;             (new Assign% [lhs temp] [rhs ast]))
         ;;       (send temp clone))]

         (cons (car e1-ret) ast)]
        
        [(is-a? ast BinExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))
         (set-field! e2 ast (cdr e2-ret))
         
         ;; (define temp (get-temp (get-field type ast) 
         ;;                        (get-field expect ast)
         ;;                        (get-field expect ast)
         ;;                        (get-field place (get-field op ast))))
         ;; (cons (list (car e1-ret) (car e2-ret)
         ;;             (new Assign% [lhs temp] [rhs ast]))
         ;;       (send temp clone))]

         (cons (append (car e1-ret) (car e2-ret)) ast)]
        
        [(is-a? ast FuncCall%)
         (define args-ret  (map (lambda (x) (send x accept this)) 
                                (get-field args ast)))
         (define new-stmts (map car args-ret))
         (define new-args  (map cdr args-ret))
         (set-field! args ast new-args)
         
         ;; only insert temp for function call for now
         (if (get-field is-stmt ast)
             (cons new-stmts ast)
             (let* ([temp (get-temp
                           (get-field type ast) 
                           (get-field expand ast)
                           (get-field expect ast)
                           (get-field place (get-field return (get-field signature ast)))
                           #t)]
                    [temp-tight (send temp clone)])
               ;; send expect = 1 so that it doesn't get expanded in desugarin step
               (set-field! expect temp-tight 1)
               (cons (list new-stmts (new Assign% [lhs temp-tight] [rhs ast] [nocomm #t]))
                     temp)))]
        
        [(or (is-a? ast VarDecl%)
             (is-a? ast ArrayDecl%))
         ast]
        
        [(is-a? ast Assign%)
         (define lhs-ret (send (get-field lhs ast) accept this))
         (define rhs-ret (send (get-field rhs ast) accept this))
         
         (set-field! lhs ast (cdr lhs-ret))
         (set-field! rhs ast (cdr rhs-ret))
         
         (list (car lhs-ret) (car rhs-ret) ast)]

	[(is-a? ast Return%)
         (define val-ret (send (get-field val ast) accept this))
         (set-field! val ast (cdr val-ret))
         (list (car val-ret) ast)]
        
        [(is-a? ast If%)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field true-block ast) accept this)
         (let ([false-block (get-field false-block ast)])
           (when false-block
             (send false-block accept this)))
         
         (set-field! condition ast (cdr cond-ret))
         
         (list (car cond-ret) ast)]
        
        [(is-a? ast While%)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field body ast) accept this)
         
         (set-field! condition ast (cdr cond-ret))
         (list (car cond-ret) ast)]
        
        [(is-a? ast For%)
         (send (get-field body ast) accept this)
         ast]
        
        [(is-a? ast FuncDecl%)
         (send (get-field body ast) accept this)
         ast]
        
        [(is-a? ast Block%)
         (set-field! stmts ast
                     (flatten (map (lambda (x) 
                                     (send x accept this))
                                   (get-field stmts ast))))
         (when (is-a? ast Program%)
           (set-field! stmts ast (append new-decls (get-field stmts ast))))
         ]
        
        [else
         (raise (format "visitor-tempinsert: unimplemented for ~a" ast))]
        
        ))))

       
