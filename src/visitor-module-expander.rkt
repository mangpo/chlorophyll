#lang racket

(require "header.rkt" "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; Expand module to multiple functions.
(define module-expander%
  (class* object% (visitor<%>)
    (super-new)

    (define debug #f)
    (define modules (make-hash))
    (define mapping #f)
    (define id #f)

    (define module-summary (make-hash))

    (define-syntax-rule (rename name)
      (if (and mapping (not (member name (list "#return" "map" "reduce"))))
          (begin
            (unless (string? name)
                    (raise (format "rename: ~a is not string" name)))
            (format "_~a_~a" id name))
          name))

    (define (fresh-place place)
      (if mapping
          (cond
           [(boolean? place) place]
           [(number? place) place] ;; TODO
           [(symbolic? place) (get-sym)]
           [(is-a? place Place%)
            (let ([at (get-field at place)])
              (if (string? at)
                  place
                  (new Place% [at (send at accept this)])))]
           [(is-a? place TypeExpansion%)
            (new TypeExpansion%
                 [place-list (fresh-place (get-field place-list place))])]
           [(is-a? place RangePlace%)
            (new RangePlace% [from (get-field from place)]
                 [to (get-field to place)]
                 [place (fresh-place (get-field place place))])]
           [(is-a? place BlockLayout%)
            (new BlockLayout% [size (get-field size place)]
                 [place-list (fresh-place (get-field place-list place))])]
           [(list? place) (map fresh-place place)]
           [(pair? place)
            (cons (fresh-place (car place)) (fresh-place (cdr place)))]
           )
          place))

    (define/public (visit ast)
      (define-syntax-rule (my x) (get-field x ast))
      (define-syntax-rule (visit-my x) (send (get-field x ast) accept this))
    
      (cond

       ;;;;;;;;;;;;;;;;;;;;;;;;;;; Modules ;;;;;;;;;;;;;;;;;;;;;;;;;;

       [(and (is-a? ast Assign%) (is-a? (get-field rhs ast) ModuleCreate%))
        (when debug (pretty-display (format "MODULE: ModuleCreate")))
        (define rhs (get-field rhs ast))
        (define lhs (get-field lhs ast))
        (define module-name (get-field name rhs))
        (define module (hash-ref modules module-name))

        (set! id (get-field name lhs))
        (set! mapping (make-hash))
        (for ([param (get-field params module)]
              [val (get-field args rhs)])
             (hash-set! mapping param val))

        (define ret
          (for/list ([stmt (get-field stmts module)])
                    (send stmt accept this)))
        (hash-set! module-summary module-name
                   (cons id (hash-ref module-summary module-name)))
        (set! mapping #f)
        
        ret
        ]

       [(is-a? ast Module%)
        (define name (get-field name ast))
        (hash-set! modules name ast)
        (hash-set! module-summary name (list))
        ]

       [(is-a? ast ModuleCall%)
        (when debug (pretty-display (format "MODULE: ModuleCall% ~a.~a"
                                            (get-field module-name ast)
                                            (get-field name ast))))
        (new FuncCall%
             [name (format "_~a_~a" (get-field module-name ast)
                           (get-field name ast))]
             [args (get-field args ast)]
             [pos (get-field pos ast)])]

       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
       [(is-a? ast Const%)
        (new Const% [n (my n)] [pos (my pos)])]

       [(is-a? ast Num%)
        (new Num% [n (visit-my n)]
             [type (my type)] [pos (my pos)])]

       [(is-a? ast Array%)
        (let ([place-type (my place-type)])
          (new Array% [name (rename (my name)) ] [sub (my sub)] [pos (my pos)]
               [index (visit-my index)]
               [place-type
                (and place-type (fresh-place (send place-type accept this)))]
               [ghost (my ghost)]))]

       [(is-a? ast Var%)
        (let ([name (my name)])
          (if (and mapping (hash-has-key? mapping name))
              (let ([sub (hash-ref mapping name)])
                (if (number? sub)
                    (new Num% [n (new Const% [n sub])] [type "int"])
                    sub))
              (new Var% [name (rename name)] [sub (my sub)]
                   [known-type (my known-type)] [pos (my pos)])))]

       [(is-a? ast Op%)
        (new Op% [op (my op)] [pos (my pos)] [place (fresh-place (my place))])]

       [(is-a? ast BinExp%)
        (new BinExp% [op (visit-my op)]
             [e1 (visit-my e1)]
             [e2 (visit-my e2)])]

       [(is-a? ast UnaExp%)
        (new UnaExp% [op (visit-my op)]
             [e1 (visit-my e1)])]

       [(is-a? ast FuncCall%)
        (new FuncCall% [name (rename (my name))]
             [args (map (lambda (x) (send x accept this)) (my args))]
             [pos (my pos)])]

       [(is-a? ast Assume%)
        (new Assume% [e1 (visit-my e1)] [pos (my pos)])]

       [(is-a? ast ReturnDecl%)
        (when debug
              (pretty-display (format "MODULE: ReturnDecl% ~a" (my var-list))))
        (new ReturnDecl% [var-list (map (lambda (x) (rename x)) (my var-list))]
             [type (my type)]
             [place (fresh-place (my place))]
             [pos (my pos)])]
       
       [(is-a? ast Param%)
        (when debug
              (pretty-display (format "MODULE: Param% ~a" (my var-list))))
        (new Param% [var-list (map (lambda (x) (rename x)) (my var-list))]
             [type (my type)]
             [place (fresh-place (my place))]
             [pos (my pos)])]

       [(is-a? ast VarDecl%)
        (when debug
              (pretty-display (format "MODULE: VarDecl% ~a" (my var-list))))
        (new VarDecl% [var-list (map (lambda (x) (rename x)) (my var-list))]
             [type (my type)]
             [place (fresh-place (my place))]
             [pos (my pos)])]

       [(is-a? ast ArrayDecl%)
        (when debug
              (pretty-display (format "MODULE: ArrayDecl% ~a" (my var))))
        (let ([init (my init)])
          (new ArrayDecl% [var (rename (my var))]
               [type (my type)] [cluster (my cluster)] [bound (my bound)]
               [init (if (is-a? init Var%) (send init accept this) init)]
               [ghost (my ghost)]
               [place-list (fresh-place (my place-list))]
               [pos (my pos)]))]

       [(is-a? ast Assign%)
        (new Assign% [lhs (visit-my lhs)]
             [rhs (visit-my rhs)] [pos (my pos)])]

       [(is-a? ast For%)
        (new For% [iter (visit-my iter)] 
             [from (my from)] [to (my to)] [body (visit-my body)]
             [pos (my pos)])]

       [(is-a? ast While%)
        (new While% [condition (visit-my condition)] [body (visit-my body)]
             [bound (my bound)] [pos (my pos)])]
       
       [(is-a? ast If%)
        (new If% [condition (visit-my condition)] 
                 [true-block (visit-my true-block)] 
                 [false-block (and (my false-block) (visit-my false-block))] 
                 [pos (my pos)])]

       [(is-a? ast Return%)
        (when debug (pretty-display (format "MODULE: Return")))
        (new Return% [val (visit-my val)] [pos (my pos)])]

       [(is-a? ast FuncDecl%)
        (when debug (pretty-display (format "MODULE: FuncDecl ~a" (my name))))
        (new FuncDecl%
             [name (rename (my name))] [args (visit-my args)] 
             [precond (visit-my precond)]
             [body (visit-my body)]
             [return (and (my return) (visit-my return))]
             [pos (my pos)])]

       [(is-a? ast Program%)
        (unless
         (empty? (my module-decls))
         (define pre (list))
         (for ([module (my module-decls)])
              (send module accept this))
         (set! pre (flatten (for/list ([module (my module-inits)])
                                             (send module accept this))))
         (set-field! module-decls ast module-summary)
         (set-field!
          module-inits ast
          (filter identity ;; filter when locations field is empty
                  (map (lambda (x)
                         (let* ([lhs (get-field lhs x)]
                                [rhs (get-field rhs x)]
                                [locations (get-field locations rhs)])
                           (and (not (empty? locations))
                                (cons (get-field name lhs) ;; instance name
                                      (get-field locations rhs)))))
                       (my module-inits))))
           
         (set-field! stmts ast
                     (append pre
                             (map (lambda (x) (send x accept this))
                                  (my stmts)))))]

       [(is-a? ast Block%)
        (new Block% [stmts (map (lambda (x) (send x accept this)) (my stmts))])]

       ))))
       
       
        
       
