#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define temp-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [count 0] [new-decls (list)])
    (define condition #f)

    (struct entry (temp type expand))

    (define (get-temp type expand expect place-type compact)
      (let* ([temp (format "_temp~a" count)]
	     [temp-decl (if (> expand 1)
			    ;; no expansion in desugar step
			    (new TempDecl% [var-list (list temp)]
				 [type (cons type expand)] ; packed type
				 [place place-type] 
				 [expect expand]
                                 [compact compact]) 
			    (new TempDecl% [var-list (list temp)]
				 [type type] ; native type
				 [place place-type] 
				 [expect expand]
                                 [compact compact]))])
	
        (set! count (add1 count))
        (set! new-decls (cons temp-decl new-decls))
        
        ;; temp for funccall:
        ;; let func() -> int::2
        ;; temp = func() 
        ;; type(temp) = (cons int 2)
        ;; expect(temp) = 1

        ;; don't set known-type
        (let* ([tmp1 (new Temp% [name temp] [type type] [expand expand] [expect expand] 
                          [place-type place-type] [compact compact])]
               [tmp2 (new Temp% [name temp] [type type] [expand expand] [expect expect]
                          [place-type place-type] [compact compact])])
        (values tmp1 tmp2))))

    (define (tempify arg param)
      (define x (send arg accept this))
      (define stmt (car x))
      (define exp (cdr x))
      (if (and (is-a? exp Var%) (regexp-match #rx"_temp" (get-field name exp)))
	  x
	  (let* ([new-temp (get-temp
			(get-field type param)
			(get-field expect param)
			(get-field expect param)
			(get-field place param) #f)]
		 [arg-temp (send new-temp clone)])
	    (cons (list stmt (new Assign% [lhs new-temp] [rhs exp] [nocomm #f]))
		  arg-temp))))

    (define/public (visit ast)
      (define (temp-stmt-exp)
        (let-values ([(tmp1 tmp2) 
                      (get-temp (get-field type ast) 
                                (get-field expect ast)
                                (get-field expect ast)
                                #f #f)])
          ;(pretty-display `(temp-stmt-exp ,(send ast to-string) ,(get-field place-type ast)))
          (values (list (new AssignTemp% [lhs tmp1] [rhs ast]))
                  tmp2)))


      (cond
        [(is-a? ast Array%)
         (define index (get-field index ast))

         ;; need to preserve index for unrolling the loop
         (define index-stmt
           (if (or (is-a? index Var%) (is-a? index Num%))
               (list)
               (let ([ret (send index accept this)])
                 (set-field! index ast (cdr ret))
                 (car ret))))

         (let-values ([(stmt exp) (temp-stmt-exp)])
           (cons (append index-stmt stmt) exp))
         ]
         
        [(or (is-a? ast Num%)
             (is-a? ast Var%))
        
         (let-values ([(stmt exp) (temp-stmt-exp)])
           (cons stmt exp))]
        
        [(is-a? ast UnaExp%)
         (define e1-ret (send (get-field e1 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))

         (let-values ([(stmt exp) (temp-stmt-exp)])
           (cons (append (car e1-ret) stmt) exp))]
        
        [(is-a? ast BinExp%)
         (define my-cond (and condition
                              (member (get-field op (get-field op ast)) 
                                      (list "<" "<=" ">=" ">" "==" "!="))))
         (set! condition #f)

         (define e1-ret (send (get-field e1 ast) accept this))
         (define e2-ret (send (get-field e2 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))
         (set-field! e2 ast (cdr e2-ret))

         (if my-cond
             (cons (append (car e1-ret) (car e2-ret)) ast)
             (let-values ([(stmt exp) (temp-stmt-exp)])
               (cons (append (car e1-ret) (car e2-ret) stmt) exp)))]
        
        [(is-a? ast FuncCall%)
	 (define params (get-field stmts (get-field args (get-field signature ast))))
	 (define tempified (map tempify (get-field args ast) params))
         (define new-stmts (map car tempified))
         (define new-args  (map cdr tempified))
         (set-field! args ast new-args)
         
         ;; only insert temp for function call for now
         (if (get-field is-stmt ast)
	     ;; return list of stmts
             (list new-stmts ast)
	     ;; return (list of stmts . ast)
             (let-values 
                 ([(tmp1 tmp2) 
                   (get-temp
                    (get-field type ast) 
                    (get-field expand ast)
                    (get-field expect ast)
                    (get-field place (get-field return (get-field signature ast)))
                    #t)])
               ;; send expect = 1 so that it doesn't get expanded in desugarin step
               (set-field! expect tmp1 1)
               (cons (list new-stmts (new AssignTemp% [lhs tmp1] [rhs ast]))
                     tmp2)))]

	[(is-a? ast Recv%)

         (let-values ([(stmt exp) (temp-stmt-exp)])
           (cons stmt exp))]

	[(is-a? ast Send%)
	 (define data-ret (send (get-field data ast) accept this))
	 (set-field! data ast (cdr data-ret))
	 (list (car data-ret) ast)]
        
        [(or (is-a? ast VarDecl%)
             (is-a? ast ArrayDecl%))
         ast]
        
        [(is-a? ast Assign%)
         (define lhs (get-field lhs ast))
         (define index-stmt
           (if (and (is-a? lhs Array%) 
                    (not (is-a? lhs Var%))
                    (not (is-a? lhs Num%)))
               (let ([ret (send lhs accept this)])
                 (set-field! index lhs (cdr ret))
                 (car ret))
               (list)))

         (define rhs-ret (send (get-field rhs ast) accept this))
         (set-field! rhs ast (cdr rhs-ret))
         
         (list index-stmt (car rhs-ret) ast)]

	[(is-a? ast Return%)
         (define val-ret (send (get-field val ast) accept this))
         (set-field! val ast (cdr val-ret))
         (list (car val-ret) ast)]
        
        [(is-a? ast If%)
         (set! condition #t)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field true-block ast) accept this)
         (let ([false-block (get-field false-block ast)])
           (when false-block
             (send false-block accept this)))
         
         (set-field! condition ast (cdr cond-ret))
         
         (list (car cond-ret) ast)]
        
        [(is-a? ast While%)
         (set! condition #t)
         (define cond-ret (send (get-field condition ast) accept this))
         (define body (get-field body ast))
         (send body accept this)
         
         (set-field! condition ast (cdr cond-ret))
         (set-field! stmts body 
                     (append 
                      (get-field stmts body) 
                      (map (lambda (x) (send x clone)) (flatten (car cond-ret)))))
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

       
