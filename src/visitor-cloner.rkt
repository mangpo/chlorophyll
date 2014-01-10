#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt" "visitor-loopbound.rkt")

(provide (all-defined-out))

;; Make a duplicate of the given AST but set the place-type of the AST
;; specific to the set range and index.
(define range-cloner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [new-funcs (list)])

    (define debug #f)

    (define env (make-hash))
    (define tempdecls (make-hash))
    (define keep #f) ;; keep symbolic-place of op% the same when _temp = a /% b;

    (define functions (make-hash))
    (define from #f)
    (define to #f)
    (define index #f)
    (define id #f)

    (define/public (set-id this-id)
      (set! id this-id))

    (define/public (add-function ast)
      (hash-set! functions (get-field name ast) ast))
    
    (define/public (set-range this-from this-to this-index this-id)
      (set! from this-from)
      (set! to this-to)
      (set! index this-index)
      (set! id this-id))

    (define/public (visit ast)
      (define (get-known-type)
        (get-field known-type ast))

      (define (get-place-type)
        (define place-type (get-field place-type ast))
        (if (place-type-dist? place-type)
            (let ([p (get-place (get-field place-type ast) index from to)])
              (if (is-a? p RangePlace%)
                  (get-field place p)
                  p))
            place-type))

      (define (fresh-place-type op)
        (if (and (not keep) 
                 (symbolic? (get-field place-type ast)) 
                 (symbolic? (get-field place op)))
            (get-sym)
	    (let ([place (get-field place op)])
	      (if (and (is-a? place Place%) 
		       (is-a? (get-field at place) Var%))
                  (let* ([var (get-field at place)]
                         [name (if (get-field sub var) 
                                   (ext-name (get-field name var) (get-field sub var))
                                   (get-field name var))])
                    ;; (pretty-display `(fresh-place-type ,(send op to-string) 
                    ;;                                    ,place
                    ;;                                    ,name
                    ;;                                    ,(hash-has-key? env name)))
                    (if (hash-has-key? env name)
                        (hash-ref env name)
                        (get-place-type)))
		  (get-place-type)))))

      (define (fresh-place place)
	(cond
         [(symbolic? place)
          (get-sym)]
         [(is-a? place TypeExpansion%)
          (new TypeExpansion% 
               [place-list (map fresh-place (get-field place-list place))])]
         [(is-a? place RangePlace%)
          (new RangePlace% [from (get-field from place)] [to (get-field to place)]
               [place (fresh-place (get-field place place))])]
         [(list? place)
          (map fresh-place place)]
         [else place]))

      (define (declare-var name place)
        (when (is-a? place TypeExpansion%)
              (let ([place-list (get-field place-list place)])
                (for ([i (in-range (length place-list))]
                      [p place-list])
                     (when debug 
                           (pretty-display (format "CLONER: declare ~a @ ~a" 
                                                   (ext-name name i) p)))
                     (declare env (ext-name name i) p))))
        (when debug (pretty-display (format "CLONER: declare ~a @ ~a" name place)))
        (declare env name place))
        
      (cond
       [(is-a? ast Const%)
        (new Const% [n (get-field n ast)] [place (get-field place ast)])]

       [(is-a? ast Num%)
        (when debug
              (pretty-display (format "CLONER: Num ~a" (send ast to-string))))
        (set! keep #f)
        (new Num% [n (send (get-field n ast) accept this)] 
             [place-type (get-place-type)])
        ]

       [(is-a? ast Array%)
        (when debug
              (pretty-display (format "CLONER: Array ~a" (send ast to-string))))
        (set! keep #f)
        (define name (get-field name ast))
	(define place (if (has-var? env name)
			  (lookup-name env name)
			  (get-place-type)))
        (new Array% [name name] 
             [type (get-field type ast)]
             [index (send (get-field index ast) accept this)]
	     [cluster (get-field cluster ast)]
             [place-type place] [known-type (get-known-type)]
             [ghost (get-field ghost ast)]
             [pos (get-field pos ast)])]

       ;; [(is-a? ast Temp%)
       ;;  (when debug
       ;;        (pretty-display (format "CLONER: Temp ~a" (send ast to-string))))
       ;;  (set! keep #f)
       ;;  (new Temp% [name (get-field name ast)]
       ;;       [type (get-field type ast)]
       ;;       [place-type (get-place-type)] [known-type (get-known-type)]
       ;;       [compact (get-field compact ast)])]

       [(is-a? ast Var%)
        (when debug
              (pretty-display (format "CLONER: Var ~a" (send ast to-string))))
        (set! keep #f)
	(define name 
          (if (is-a? ast VarDup%)
              (format "~ap~a" (get-field name ast) id)
              (get-field name ast)))
	(define place (if (has-var? env name)
			  (lookup-name env name)
			  (get-place-type)))
        (new (if (is-a? ast Temp%) Temp% Var%)
             [name name]
             [type (get-field type ast)]
             [compact (get-field compact ast)]
             [place-type place] [known-type (get-known-type)])]

       [(is-a? ast Op%)
        (new Op% [op (get-field op ast)] [place (get-field place ast)])]

       [(is-a? ast UnaExp%)
        (when debug
              (pretty-display (format "CLONER: UnaExp ~a" (send ast to-string))))
        (define op-ret (send (get-field op ast) accept this))
        (define place-type (fresh-place-type op-ret))
        (when (and (not keep) (symbolic? (get-field place op-ret)))
              (set-field! place op-ret place-type))
        (set! keep #f)
        (new UnaExp% 
             [op op-ret]
             [e1 (send (get-field e1 ast) accept this)]
             [type (get-field type ast)]
             [known-type (get-known-type)]
             [place-type place-type])]

       [(is-a? ast BinExp%)
        (define op-ret (send (get-field op ast) accept this))
        (define place-type (fresh-place-type op-ret))
        (when (and (not keep) (symbolic? (get-field place op-ret)))
              (set-field! place op-ret place-type))

        (when debug
              (pretty-display (format "CLONER: BinExp ~a @ ~a" 
                                      (send ast to-string)
                                      place-type)))
        (set! keep #f)
        (new BinExp% 
             [op op-ret]
             [e1 (send (get-field e1 ast) accept this)]
             [e2 (send (get-field e2 ast) accept this)]
             [type (get-field type ast)]
             [known-type (get-known-type)]
             [place-type place-type])]

       [(is-a? ast FuncCallDup%)
        (when debug (pretty-display (format "CLONER: FuncCallDup ~a" (send ast to-string))))
        (set! keep #f)
	
	(define func-name (get-field name ast))
	(define new-func (send (hash-ref functions func-name) accept this))
	(define new-name (format "_~a~a" id func-name))
	(set-field! name new-func new-name)
        (set! new-funcs (cons new-func new-funcs))

	(define return-place (and (get-field return new-func)
				  (get-field place (get-field return new-func))))
        (new FuncCall%
             [name new-name]
             [args (map (lambda (x) (send x accept this)) (get-field args ast))]
             [type (get-field type ast)]
             [signature new-func]
             [known-type (get-known-type)]
             [place-type (typeexpansion->list return-place)])]

       [(is-a? ast FuncCall%)
        (set! keep #f)
        (new FuncCall%
             [name (get-field name ast)]
             [args (map (lambda (x) (send x accept this)) (get-field args ast))]
             [type (get-field type ast)]
             [signature (get-field signature ast)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast ProxyReturn%)
        (new ProxyReturn% [place-type (get-field place-type ast)])]

       [(is-a? ast Param%)
	(define place (fresh-place (get-field place ast)))
	(for ([var (get-field var-list ast)])
	     (declare env var place))
        (new Param%
             [var-list (get-field var-list ast)] ;; not copy
             [type (get-field type ast)]
             [known (get-field known ast)]
             [place place]
	     [place-type place])]

       [(is-a? ast VarDecl%)
        (pretty-display (format "CLONER: VarDecl% ~a @ ~a (before)" (get-field var-list ast)
                                (get-field place ast)))
	(define place (fresh-place (get-field place ast)))
        (define new-var-list
          (for/list ([var (get-field var-list ast)])
            (let ([name (if (is-a? ast VarDeclDup%) (format "~ap~a" var id) var)])
              (declare-var name place)
              name
              )))
        
        (define ret
          (new (cond
                [(is-a? ast ReturnDecl%) ReturnDecl%]
                [(is-a? ast TempDecl%) TempDecl%]
                [else VarDecl%])
               [var-list new-var-list]
               [type (get-field type ast)]
               [known (get-field known ast)]
               [place place]))
        (pretty-display (format "CLONER: VarDecl% ~a @ ~a (ret)" (get-field var-list ast)
                                place))
        (send ret pretty-print)

        (when (is-a? ast TempDecl%)
          (for/list ([var (get-field var-list ast)])
                    (hash-set! tempdecls var ret)))
        
        ret
        ]
             
       [(is-a? ast ArrayDecl%)
        ;; for privatization
        (define place (fresh-place (get-field place-list ast)))
        (define name (get-field var ast))
        (if (list? place)
            (begin
              (unless (= (length place) 1)
                      (raise (format "Array declaration inside loop cannot be distributed. Error at ~a in line ~a." (get-field var ast) (send ast get-line))))
              (declare-var name (get-field place (car place))))
            (declare-var name place))

        (new ArrayDecl%
             [var (get-field var ast)]
             [type (get-field type ast)]
             [known (get-field known ast)]
             [bound (get-field bound ast)]
             [init (get-field init ast)]
	     [cluster (get-field cluster ast)]
             [place-list place])]

       [(is-a? ast For%)
        (new For%
             [iter (send (get-field iter ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [from (get-field from ast)]
             [to (get-field to ast)]
             [known (get-field known ast)]
             [unroll (get-field unroll ast)]
             [place-list (get-field place-list ast)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [(is-a? ast If%)   
	(let ([false-block (get-field false-block ast)])
	  (get-new-if ast
		      (send (get-field condition ast) accept this)
		      (send (get-field true-block ast) accept this)
		      (and false-block (send false-block accept this))
		      (get-field body-placeset ast)))]

       [(is-a? ast While%)
	(get-new-while ast
		       (send (get-field condition ast) accept this)
		       (send (get-field body ast) accept this)
		       (get-field bound ast)
		       (get-field body-placeset ast)
		       (send (get-field pre ast) accept this))]

       [(is-a? ast AssignTemp%)
        (define (expand p n)
          (cond 
           [(list? p)
            (list->typeexpansion p)]
           [(rosette-number? p)
            (new TypeExpansion% [place-list (for/list ([i (in-range n)]) p)])]
           [else
            (raise (format "visitor-cloner: AssignTemp% unimplemented for ~a" p))]))

        (define lhs-ret (send (get-field lhs ast) accept this))
        (set! keep #t)
        (define rhs-ret (send (get-field rhs ast) accept this))

        (define temp-name (get-field name lhs-ret))
        (when (hash-has-key? tempdecls temp-name)
              (define decl (hash-ref tempdecls temp-name))
              ;; (define type-expand (cdr (get-field type decl)))
              (pretty-display "HERE!!!!!!!!!!!!!!")
              (pretty-display (get-field place-type rhs-ret))
              (define place (expand (get-field place-type rhs-ret) 
                                    (cdr (get-field type decl))))
              (set-field! place-type lhs-ret place)
              (set-field! place decl place)
              (declare-var temp-name place))
        
        (new AssignTemp%
               [lhs lhs-ret]
               [rhs rhs-ret])]

       [(is-a? ast Assign%)
        (new Assign%
             [lhs (send (get-field lhs ast) accept this)]
             [rhs (send (get-field rhs ast) accept this)])]

       [(is-a? ast Return%)
	(define val (get-field val ast))
        (new Return%
             [val (if (list? val) 
		      (map (lambda (x) (send x accept this)) val)
		      (send val accept this))]
	     [expect (get-field expect ast)])]

       [(is-a? ast Block%)
        (new Block%
             [stmts (map (lambda (x) (send x accept this)) (get-field stmts ast))])]

       [(is-a? ast FuncDecl%)
        (new FuncDecl%
             [name (get-field name ast)]
             [args (send (get-field args ast) accept this)]
             [return (and (get-field return ast) 
                          (send (get-field return ast) accept this))]
             [body (send (get-field body ast) accept this)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [else
        (raise (format "visitor-cloner: unimplemented for ~a" ast))]

       ))))
             
