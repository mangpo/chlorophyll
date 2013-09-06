#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(define temp-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [count 0] [new-decls (list)])
    (define debug #f)

    (define/public (visit ast)
     (cond
        [(or (is-a? ast Num%)
             (is-a? ast Var%))
         (when debug (pretty-display (format "TEMPINSERT: ~a" (send ast to-string))))
         (cons (list) ast)
	 ]
        
        [(is-a? ast UnaExp%)
         (when debug (pretty-display (format "TEMPINSERT: ~a" (send ast to-string))))
         (define e1-ret (send (get-field e1 ast) accept this))
         (cons (car e1-ret) ast)
	 ]
        
        [(is-a? ast BinExp%)
         (when debug (pretty-display (format "TEMPINSERT: ~a" (send ast to-string))))
         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))
         (set-field! e2 ast (cdr e2-ret))
         (cons (append (car e1-ret) (car e2-ret)) ast)]
        
        [(is-a? ast FuncCall%)
         (when debug (pretty-display (format "TEMPINSERT: FuncCall ~a" (send ast to-string))))

         ;; my-arg is ture if this funcall is an argument to another funccall.
         (define signature (get-field signature ast))
         ;; (define return-place (and (get-field return signature)
         ;;                           (get-field place (get-field return signature))))
	 (define params (get-field stmts (get-field args (get-field signature ast))))
	 (define tempified (map (lambda (x) (send x accept this)) 
                                (get-field args ast) params))
         (define new-stmts (map car tempified))

         (define new-args
           (for/list ([arg (map cdr tempified)])
             (if (get-field might-need-storage arg)
                 (let ([mismatch (count (lambda (x y) (not (equal? x y)))
                                        (get-field return arg)
                                        (take param (length (get-field return arg))))])
                   (if (> mismatch 0)
                       (insertemp) ;; TODO
                       arg))
                 arg)))
           
              
         (set-field! args ast new-args)
         (cons new-stmts ast)]
               

	[(is-a? ast Recv%)
         (cons (list) ast)
	 ]

	[(is-a? ast Send%)
	 (set! is-arg #f)
	 (define data-ret (send (get-field data ast) accept this))
	 (set-field! data ast (cdr data-ret))
	 (list (car data-ret) ast)]
        
        [(or (is-a? ast VarDecl%)
             (is-a? ast ArrayDecl%))
         ast]
        
        [(is-a? ast Assign%)
         (when debug (pretty-display (format "TEMPINSERT: Assign")))
	 (set! is-arg #f)
         (define lhs-ret (send (get-field lhs ast) accept this))
         (define rhs-ret (send (get-field rhs ast) accept this))
         
         (set-field! lhs ast (cdr lhs-ret))
         (set-field! rhs ast (cdr rhs-ret))
         
         (list (car lhs-ret) (car rhs-ret) ast)]

	[(is-a? ast Return%)
	 (set! is-arg #f)
         (define val-ret (send (get-field val ast) accept this))
         (set-field! val ast (cdr val-ret))
         (list (car val-ret) ast)]
        
        [(is-a? ast If%)
	 (set! is-arg #f)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field true-block ast) accept this)
         (let ([false-block (get-field false-block ast)])
           (when false-block
             (send false-block accept this)))
         
         (set-field! condition ast (cdr cond-ret))
         
         (list (car cond-ret) ast)]
        
        [(is-a? ast While%)
	 (set! is-arg #f)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field body ast) accept this)
         
	 (set-field! stmts (get-field pre ast) (flatten (car cond-ret)))
         (set-field! condition ast (cdr cond-ret))
         (list ast)]
        
        [(is-a? ast For%)
         (send (get-field body ast) accept this)
         ast]
        
        [(is-a? ast FuncDecl%)
         (when debug (pretty-display (format "TEMPINSERT: FuncDecl ~a" (get-field name ast))))
         (define body (get-field body ast))
         (define decl-block (car (get-field stmts body)))
         (send body accept this)
         (set-field! stmts decl-block (append (get-field stmts decl-block) new-decls))
         (set! new-decls (list))
         ast]
        
        [(is-a? ast Block%)
         (set-field! stmts ast
                     (flatten (map (lambda (x) 
                                     (send x accept this))
                                   (get-field stmts ast))))
         (when (is-a? ast Program%)
           (set-field! stmts ast (append new-decls (get-field stmts ast))))
         ast
         ]
        
        [else
         (raise (format "visitor-tempinsert: unimplemented for ~a" ast))]
        
        ))))