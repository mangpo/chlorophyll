#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

(define temp-inserter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [count 0] [new-decls (list)])
    (define condition #f)
    (define current-p #f)

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

    (define/public (visit ast)
      (define (temp-stmt-exp my-p)
        (let-values ([(tmp1 tmp2) 
                      (get-temp (get-field type ast) 
                                (get-field expect ast)
                                (get-field expect ast)
                                my-p #f)])
          ;(pretty-display `(temp-stmt-exp ,(send ast to-string) ,(get-field place-type ast)))
          (values (list (new Assign% [lhs tmp1] [rhs ast]))
                  tmp2)))


      (cond
        [(is-a? ast Array%)
	 (define my-p current-p)
	 (set! current-p #f)
         (define index (get-field index ast))

         ;; need to preserve index for unrolling the loop
         (define index-stmt
           (if (or (is-a? index Var%) (is-a? index Num%))
               (list)
               (let ([ret (send index accept this)])
                 (set-field! index ast (cdr ret))
                 (car ret))))

	 (if my-p
	     (let-values ([(stmt exp) (temp-stmt-exp my-p)])
	       (cons (append index-stmt stmt) exp))
	     (cons index-stmt ast))
         ]
         
        [(or (is-a? ast Num%)
             (is-a? ast Var%))
	 (define my-p current-p)
	 (set! current-p #f)
	 (if my-p
	     (let-values ([(stmt exp) (temp-stmt-exp my-p)])
	       (cons stmt exp))
	     (cons (list) ast))
	 ]
        
        [(is-a? ast UnaExp%)
	 (define my-p current-p)
	 (set! current-p #f)

         (define e1-ret (send (get-field e1 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))

	 (if my-p
	     (let-values ([(stmt exp) (temp-stmt-exp my-p)])
	       (cons (append (car e1-ret) stmt) exp))
	     (cons (car e1-ret) ast))
	 ]
        
        [(is-a? ast BinExp%)
	 (define my-p current-p)
	 (define place (get-field place (get-field op ast)))

         ;; (define my-cond (and condition
         ;;                      (member (get-field op (get-field op ast)) 
         ;;                              (list "<" "<=" ">=" ">" "==" "!="))))
         (set! condition #f)

	 (set! current-p place)
         (define e1-ret (send (get-field e1 ast) accept this))
	 (set! current-p place)
         (define e2-ret (send (get-field e2 ast) accept this))
         (set-field! e1 ast (cdr e1-ret))
         (set-field! e2 ast (cdr e2-ret))

         (if my-p
             (let-values ([(stmt exp) (temp-stmt-exp my-p)])
               (cons (append (car e1-ret) (car e2-ret) stmt) exp))
             (cons (append (car e1-ret) (car e2-ret)) ast))]
        
        [(is-a? ast FuncCall%)
	 (pretty-display (format "TEMPINSERT: FuncCall ~a" (get-field name ast)))
	 (define (tempify arg param)
	   (set! current-p (get-field place param)) ;; check
	   (pretty-display (format "  param:~a place:~a" (get-field var-list param) current-p))
	   (send arg accept this))
	   ;; (define x (send arg accept this))
	   ;; (define stmt (car x))
	   ;; (define exp (cdr x))
	   ;; (if (and (is-a? exp Var%) (regexp-match #rx"_temp" (get-field name exp)))
	   ;;     x
	   ;;     (let* ([new-temp (get-temp
	   ;; 			 (get-field type param)
	   ;; 			 (get-field expect param)
	   ;; 			 (get-field expect param)
	   ;; 			 (get-field place param) #f)]
	   ;; 	      [arg-temp (send new-temp clone)])
	   ;; 	 (cons (list stmt (new Assign% [lhs new-temp] [rhs exp] [nocomm #f]))
	   ;; 	       arg-temp))))

	 (define my-p current-p)
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
		    (if my-p my-p
			(get-field place (get-field return (get-field signature ast))))
                    #t)])
               ;; send expect = 1 so that it doesn't get expanded in desugarin step
               (set-field! expect tmp1 1)
               (cons (list new-stmts (new AssignTemp% [lhs tmp1] [rhs ast]))
                     tmp2)))]

	[(is-a? ast Recv%)
	 (define my-p current-p)
         (if my-p
	     (let-values ([(stmt exp) (temp-stmt-exp my-p)])
	       (cons stmt exp))
	     (cons (list) ast))
	 ]

	[(is-a? ast Send%)
	 (set! current-p #f)
	 (define data-ret (send (get-field data ast) accept this))
	 (set-field! data ast (cdr data-ret))
	 (list (car data-ret) ast)]
        
        [(or (is-a? ast VarDecl%)
             (is-a? ast ArrayDecl%))
         ast]
        
        [(is-a? ast Assign%)
	 (set! current-p #f)
         (define lhs (get-field lhs ast))
         (define index-stmt
           (if (and (is-a? lhs Array%) 
                    (not (is-a? lhs Var%))
                    (not (is-a? lhs Num%)))
               (let ([ret (send lhs accept this)])
                 (set-field! index lhs (cdr ret))
                 (car ret))
               (list)))

	 (set! current-p #f)
         (define rhs-ret (send (get-field rhs ast) accept this))
         (set-field! rhs ast (cdr rhs-ret))
         
         (list index-stmt (car rhs-ret) ast)]

	[(is-a? ast Return%)
	 (set! current-p #f)
         (define val-ret (send (get-field val ast) accept this))
         (set-field! val ast (cdr val-ret))
         (list (car val-ret) ast)]
        
        [(is-a? ast If%)
	 (set! current-p #f)
         (set! condition #t)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field true-block ast) accept this)
         (let ([false-block (get-field false-block ast)])
           (when false-block
             (send false-block accept this)))
         
         (set-field! condition ast (cdr cond-ret))
         
         (list (car cond-ret) ast)]
        
        [(is-a? ast While%)
	 (set! current-p #f)
         (set! condition #t)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field body ast) accept this)
         
	 (set-field! stmts (get-field pre ast) (car cond-ret))
         (set-field! condition ast (cdr cond-ret))
         (list ast)]
        
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

       
