#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; Insert temp for a function call whose return type is tuple 
;; that IS an argument to another function.
;; When 'replace-all' is #t, replace all those function calls with temps.
;; When 'replace-all' is #f, replace only those function calls 
;; whose one of the return places doesn't match the place of that particulare param.
(define temp-inserter2%
  (class* object% (visitor<%>)
    (super-new)
    (init-field replace-all [temp-count 0] [new-decls (list)])
    (define debug #f)

    (define (get-temp type expand place-list place-type)
      (let* ([temp (format "_temp2_~a" temp-count)]
	     [temp-decl (new TempDecl% [var-list (list temp)]
				 [type (cons type expand)] ; packed type
				 [place place-type] [compact #t])])
	
        (set! temp-count (add1 temp-count))
        (set! new-decls (cons temp-decl new-decls))

        ;; don't set known-type
        (let* ([tmp (new Temp% [name temp] [type type] [place-type place-type]
                         [compact #t])]
               [tmp-list (for/list ([p place-list]
                                    [i (in-range (length place-list))])
                                   (new Temp% [name temp] [type type] [sub i]
                                        [place-type (get-field place-type p)]
                                        [compact #t]))])
          (values tmp tmp-list))))

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
         (define place-type (get-field place-type ast))
         (when (and (list? place-type) (= (length place-type) 1))
               (set-field! place-type ast (get-field place-type (car place-type))))
         
	 (define tempified (map (lambda (x) (send x accept this)) 
                                (get-field args ast)))
         (define new-stmts (map car tempified))

         (define (new-arg-list arg)
           (define place-group (list->typeexpansion (get-field place-type arg)))
           (define len (length (get-field place-type arg)))
           (define-values (lhs arg-list) (get-temp (get-field type arg) 
                                                   len 
                                                   (get-field place-type arg)
                                                   place-group))
           (define assign (new AssignTemp% [lhs lhs] [rhs arg]))
           (set! new-stmts (append new-stmts (list assign)))
           arg-list)

         (define new-args
           (cond
            [replace-all
             (flatten
              (for/list ([arg (map cdr tempified)])
                 (if (and (is-a? arg FuncCall%) (list? (get-field place-type arg)))
                     (new-arg-list arg)
                     arg)))]
             [else
              (let* ([signature (get-field signature ast)]
                     [params (get-field stmts (get-field args (get-field signature ast)))])
                (flatten
                 (for/list ([arg (map cdr tempified)])
                   (if (and (is-a? arg FuncCall%) (list? (get-field place-type arg)))
                       (let* ([len (length (get-field place-type arg))]
                              [mismatch (count (lambda (x y) 
                                                 (not (equal? (get-field place-type x)
                                                              (get-field place-type y))))
                                               (get-field place-type arg)
                                               (take params len))])
                         (if (> mismatch 0)
                             (begin
                               (set! params (drop params len))
                               (new-arg-list arg))
                             (begin
                               (set! params (cdr params))
                               arg)))
                       (begin
                         (set! params (cdr params))
                         arg)))))]))
         
         (set-field! args ast new-args)
         (cons new-stmts ast)]
               
	[(is-a? ast Recv%)
         (when debug (pretty-display (format "TEMPINSERT: Recv")))
         (cons (list) ast)
	 ]

	[(is-a? ast Send%)
         (when debug (pretty-display (format "TEMPINSERT: Send")))
	 (define data-ret (send (get-field data ast) accept this))
	 (set-field! data ast (cdr data-ret))
	 (list (car data-ret) ast)]
        
        [(or (is-a? ast VarDecl%)
             (is-a? ast ArrayDecl%))
         (when debug (pretty-display (format "TEMPINSERT: VarDecl & ArrayDecl")))
         ast]
        
        [(is-a? ast Assign%)
         (when debug (pretty-display (format "TEMPINSERT: Assign ~a ~a"
                                             (get-field lhs ast)
                                             (get-field rhs ast)))
               (send ast pretty-print)
               )
         (define lhs-ret (send (get-field lhs ast) accept this))
         (define rhs-ret (send (get-field rhs ast) accept this))
         
         (set-field! lhs ast (cdr lhs-ret))
         (set-field! rhs ast (cdr rhs-ret))
         
         (list (car lhs-ret) (car rhs-ret) ast)]

	[(is-a? ast Return%)
         (when debug (pretty-display (format "TEMPINSERT: Return")))
         (list ast)]
        
        [(is-a? ast If%)
         (when debug (pretty-display (format "TEMPINSERT: If")))
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field true-block ast) accept this)
         (let ([false-block (get-field false-block ast)])
           (when false-block
             (send false-block accept this)))
         
         (set-field! condition ast (cdr cond-ret))
         
         (list (car cond-ret) ast)]
        
        [(is-a? ast While%)
         (when debug (pretty-display (format "TEMPINSERT: While")))
         (define pre (get-field pre ast))
         (send pre accept this)
         (define cond-ret (send (get-field condition ast) accept this))
         (send (get-field body ast) accept this)
         
	 (set-field! stmts (get-field pre ast) 
                     (append (get-field stmts pre) (flatten (car cond-ret))))

         (set-field! condition ast (cdr cond-ret))
         (list ast)]
        
        [(is-a? ast For%)
         (when debug (pretty-display (format "TEMPINSERT: For")))
         (send (get-field body ast) accept this)
         ast]
        
        [(is-a? ast FuncDecl%)
         (when debug (pretty-display (format "TEMPINSERT: FuncDecl ~a" (get-field name ast)))
               )
         (define body (get-field body ast))
         (send body accept this)
         (define parts (get-field stmts body))
         (if (and (not (empty? parts)) (is-a? (car parts) Block%))
             ;; if body is sepearated into 2 parts: var decls, and statements
             (let ([decl-block (car parts)])
               (set-field! stmts decl-block
                           (append (get-field stmts decl-block) new-decls)))
             (set-field! stmts body (append new-decls parts)))
         (set! new-decls (list))
         ast]
        
        [(is-a? ast Block%)
         (when debug (pretty-display (format "TEMPINSERT: Block"))
               )
         (set-field! stmts ast
                     (flatten (map (lambda (x) 
                                     (send x accept this))
                                   (get-field stmts ast))))
         (when (is-a? ast Program%)
           (set-field! stmts ast (append new-decls (get-field stmts ast))))
         ast
         ]
        
        [else
         (raise (format "visitor-tempinsert2: unimplemented for ~a" ast))]
        
        ))))