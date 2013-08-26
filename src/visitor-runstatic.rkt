#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "ast-util.rkt"
         "parser.rkt"
         "visitor-interface.rkt")

(provide static-runner%)

(struct ret (previous-filters all-filters all-funcs)
  #:constructor-name make-ret)

(define static-runner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [env (make-hash)] ;; map varname -> (type, value)
                )

    (define debug #f)

    (define (push-scope)
      ;(pretty-display `(push-scope))
      (let ([new-env (make-hash)])
        (dict-set! new-env "__up__" env)
        (set! env new-env)))

    (define (pop-scope)
      ;(pretty-display `(pop-scope))
      (set! env (dict-ref env "__up__")))
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Const%) (cons "int" (get-field n ast))]
       [(is-a? ast Num%) (get-field n ast)]

       [(is-a? ast Op%)
        (case (get-field op ast)
          ;; TODO: Type checks (are they even necessary? Ask Mangpo)
          [("+") (λ (v1 v2) (cons "int" (+ (cdr v1) (cdr v2))))]
          [("-") (λ (v1 v2) (cons "int" (- (cdr v1) (cdr v2))))]
          ;; ...
          )]

       [(is-a? ast Array%)
        (raise "visitor-runstatic: arrays not supported yet. TODO!")]

       [(is-a? ast Var%)
        (lookup env ast)]

       [(is-a? ast UnaExp%)
        (define e1 (get-field e1 ast))
        (define op (get-field op ast))
        
        (define v1 (send e1 accept this))
        (define op-func (send op accept this))
        
        (op-func v1)]
       
       [(is-a? ast BinExp%)
        (define e1 (get-field e1 ast))
        (define e2 (get-field e2 ast))
        (define op (get-field op ast))
        
        (define v1 (send e1 accept this))
        (define v2 (send e2 accept this))
        (define op-func (send op accept this))
        
        (op-func v1 v2)]

       [(is-a? ast VarDecl%)
        (define type (car (get-field place-type ast)))
        (define var-list (get-field var-list ast))
        
        ;; put vars into env
        (for ([var var-list])
          (declare env var (cons type (void))))
        
        (make-ret (list) (list) (list))]

       [(is-a? ast ArrayDecl%)
        (raise "visitor-runstatic: arrays not supported yet. TODO!")]

       [(is-a? ast For%)
        (raise "visitor-runstatic: for loops not supported yet. TODO!")]

       [(is-a? ast Forever%)
        (raise "visitor-runstatic: forever loops not supported yet 
               (ever? same as while(1), etc. though) TODO!")]

       [(is-a? ast If%)
        (raise "visitor-runstatic: if statements not supported yet. TODO!")]

       [(is-a? ast While%)
        (raise "visitor-runstatic: while loops not supported yet. TODO!")]

       [(is-a? ast Assign%)
        (define lhs (get-field lhs ast))
        (define rhs (get-field rhs ast))
        
        (define var (send lhs accept this))
        (define value (send rhs accept this))
        (update env var value)
        
        (make-ret (list) (list) (list))]

       [(is-a? ast Return%)
        (raise "visitor-runstatic: return not supported yet. TODO!")]

       [(is-a? ast Program%)
        (when debug (pretty-display "RUNSTATIC: Program"))
        (define stmts (get-field stmts ast))
        
        ;; update env front to back
        (for ([stmt stmts]
              #:when (is-a? stmt CallableDecl%))
          (declare env (get-field name stmt) stmt))
        
        (define main (lookup-name env "Main"))
        (define input-filter
          (get-global-input-filter (get-field input-type main)))
        (define output-filter
          (get-global-output-filter (get-field output-type main)))

        (declare env "__previous__" (list input-filter))
        (declare env "__method__" 'pipeline)
        (define ret
          (send (new Block% [stmts (list (get-add-call main) output-filter)])
                accept this))

        ;; remove static declarations
        (set-field! stmts ast
          (for/list ([stmt stmts]
                     #:unless (is-a? stmt AbstractFilterDecl%)
                     #:unless (is-a? stmt StaticCallableDecl%))
            stmt))
        
        ;; add concrete filter declarations
        (append-field! stmts ast
                       (drop-right (ret-all-filters ret) 1)
                       (ret-all-funcs ret))
        
	(void)]

       [(is-a? ast Block%) 
        (when debug (pretty-display "RUNSTATIC: Block"))
        (define previous-filters (list))
        (define all-filters (list))
        (define all-funcs (list))

        (declare env "__previous_so_far__" (list))
        (for ([stmt (get-field stmts ast)])
          (define ret (send stmt accept this))
          (append! all-filters (ret-all-filters ret))
          (append! all-funcs (ret-all-funcs ret))
          (cond
            [(equal? (lookup-name env "__method__") 'pipeline)
             (update-name env "__previous__" (ret-previous-filters ret))
             (set! previous-filters (ret-previous-filters ret))
             ]
            [(equal? (lookup-name env "__method__") 'splitjoin)
             (append-name env "__previous_so_far__" (ret-previous-filters ret))
             (append! previous-filters (ret-previous-filters ret))
             ]
          ))

        (make-ret previous-filters all-filters all-funcs)]

       [(is-a? ast PipelineDecl%)
        (when debug (pretty-display (format "RUNSTATIC: PipelineDecl ~a" (get-field name ast))))
        (push-scope)

        (declare env "__method__" 'pipeline)
        (define ret (send (get-field body ast) accept this))

        (pop-scope)
        ret]
       
       [(is-a? ast SplitJoinDecl%)
        (when debug (pretty-display (format "RUNSTATIC: SplitJoinDecl ~a" (get-field name ast))))
        (push-scope)
        (declare env "__input_type__" (get-field input-type ast))
        (declare env "__output_type__" (get-field output-type ast))

        (declare env "__method__" 'splitjoin)
        (define ret (send (get-field body ast) accept this))
        (update-name env "__previous__" (ret-previous-filters ret))

        (define join (lookup-name env "__join__"))
        (define split (lookup-name env "__split__"))

        (define n (length (ret-previous-filters ret)))
        (set-field! body split (get-roundrobin-split-body n))
        (set-field! body join (get-roundrobin-join-body n))

        (pop-scope)
        (make-ret (list join)
                  (append (ret-all-filters ret))
                  (append (ret-all-funcs ret)))]

       [(is-a? ast RoundRobinSplit%)
        (when debug (pretty-display "RUNSTATIC: RoundRobinSplit"))

        (define split (get-empty-filter (lookup-name env "__input_type__")
                                        (get-field place ast)))
        (define ret (send split accept this))
        (update-name env "__previous__" (ret-previous-filters ret))
        (declare env "__split__" split)

        (make-ret (list) (ret-all-filters ret) (ret-all-funcs ret))]

       [(is-a? ast RoundRobinJoin%)
        (when debug (pretty-display "RUNSTATIC: RoundRobinJoin"))

        (update-name env "__previous__" (lookup-name env "__previous_so_far__"))
        (define join (get-empty-filter (lookup-name env "__output_type__")
                                       (get-field place ast)))
        (define ret (send join accept this))
        (update-name env "__previous__" (ret-previous-filters ret))
        (declare env "__join__" join)

        (make-ret (list) (ret-all-filters ret) (ret-all-funcs ret))]

       [(is-a? ast Add%)
        (when debug (pretty-display (format "RUNSTATIC: Add ~a" ast)))
        (define call (get-field call ast))
        (define decl (get-field signature call))
        (define arg-values (map (λ (exp) (send exp accept this))
                                (get-field args call)))
        
        
        (cond
          [(is-a? decl AbstractFilterDecl%)
           (define new-filter (new ConcreteFilterDecl%
                               [abstract decl]
                               [arg-values arg-values]
                               ;;
                               [name (get-field name decl)]
                               [input-vardecl (get-field input-vardecl decl)]
                               [output-vardecl (get-field output-vardecl decl)]
                               [args (get-field args decl)]
                               [body (get-field body decl)]
                               ))
           (send new-filter accept this)]
          [(is-a? decl PipelineDecl%)
           (send decl accept this)]
          [(is-a? decl SplitJoinDecl%)
           (send decl accept this)]
          [(is-a? decl FuncDecl%)
           (raise (format "visitor-runstatic: tried to add function as a stream in ~a" ast))]
          [else
           (raise (format "visitor-runstatic: unimplemented add call to ~a" decl))])
       ]

       [(is-a? ast ConcreteFilterDecl%)
        (when debug (pretty-display (format "RUNSTATIC: ConcreteFilterDecl ~a" (get-field name ast))))
        (define new-filter ast)

        (define all-output-funcs (list))
        (for ([previous-filter (lookup-name env "__previous__")])
          (append-field! output-filters previous-filter (list new-filter))
          (define output-funcs
            (if (is-a? new-filter GlobalIOConcreteFilterDecl%)
              (list (get-output-func-push new-filter))
              (list (get-output-func-make-available previous-filter new-filter))
              ))
          (append-field! output-funcs previous-filter output-funcs)
          (set! all-output-funcs (append all-output-funcs output-funcs))
          )

        (set-field! input-filters new-filter (lookup-name env "__previous__"))
        (define all-input-funcs
          (for/list ([previous-filter (lookup-name env "__previous__")])
            (if (is-a? previous-filter GlobalIOConcreteFilterDecl%)
              (get-input-func-pull previous-filter)
              (get-input-func-made-available new-filter previous-filter)
              )))
        (set-field! input-funcs new-filter all-input-funcs)

        (for ([output-func all-output-funcs]
              [input-func all-input-funcs])
          (when (is-a? input-func FilterIOFuncDecl%)
            (set-field! source-output-func input-func output-func))
          (when (is-a? output-func FilterIOFuncDecl%)
            (set-field! destination-input-func output-func input-func))
          )

        (make-ret (list new-filter) (list new-filter) (append all-input-funcs all-output-funcs))
        ]
       
       [(is-a? ast FuncDecl%)
        (raise "visitor-runstatic: function declarations not supported yet. TODO!")]
       
       [(is-a? ast FuncCall%)
        (raise "visitor-runstatic: function calls inside static code is not supported yet, TODO!")]
       
       [else (raise (format "visitor-runstatic: unimplemented for ~a" ast))]))
))
