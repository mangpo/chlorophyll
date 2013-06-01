#lang racket

(require "header.rkt"
         "ast.rkt" "ast-util.rkt" "visitor-interface.rkt")

(provide (all-defined-out))

;; Make a duplicate of the given AST but set the place-type of the AST
;; specific to the set range and index.
(define range-cloner%
  (class* object% (visitor<%>)
    (super-new)
    (init-field [range #f] [index #f])

    (define/public (set-range x-range x-index)
      (set! range x-range)
      (set! index x-index))

    (define/public (visit ast)
      (define (get-known-type)
        (get-field known-type ast))

      (define (get-place-type)
        (let ([place-type (get-field place-type ast)])
          (if (and (place-type-dist? place-type) 
                   (equal? (get-field name (cdr place-type)) index))
              (ormap (lambda (x) 
                       ;; return x that covers the given range
                       (and (and (<= (get-field from x) (get-field from range))
                                (>= (get-field to x) (get-field to range)))
			    (get-field place x))) 
                     (car place-type))
              place-type)))
        
      (define (get-place-offset)
        (let ([place-type (get-field place-type ast)])
          (if (and (place-type-dist? place-type) 
                   (equal? (get-field name (cdr place-type)) index))
              (ormap (lambda (x) 
                       ;; return x that covers the given range
                       (and (and (<= (get-field from x) (get-field from range))
                                (>= (get-field to x) (get-field to range)))
			    (cons (get-field place x) (get-field from x)))) 
                     (car place-type))
              (cons place-type 0))))

      (cond
       [(is-a? ast Const%)
        (new Const% [n (get-field n ast)] [place (get-field place ast)])]

       [(is-a? ast Num%)
        (new Num% [n (send (get-field n ast) accept this)] 
             [place-type (get-place-type)])
        ]

       [(is-a? ast Array%)
	(define place-offset (get-place-offset))
        (new Array% [name (get-field name ast)] 
             [index (send (get-field index ast) accept this)]
	     [offset (cdr place-offset)] ;; need this to substract from the index
             [place-type (car place-offset)] [known-type (get-known-type)])]

       [(is-a? ast Var%)
        (new Var% [name (get-field name ast)]
             [place-type (get-place-type)] [known-type (get-known-type)])]

       [(is-a? ast Op%)
        (new Op% [op (get-field op ast)] [place (get-field place ast)])]

       [(is-a? ast UnaExp%)
        (new UnaExp% 
             [op (send (get-field op ast) accept this)]
             [e1 (send (get-field e1 ast) accept this)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast BinExp%)
        (new BinExp% 
             [op (send (get-field op ast) accept this)]
             [e1 (send (get-field e1 ast) accept this)]
             [e2 (send (get-field e2 ast) accept this)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast FuncCall%)
        (new FuncCall%
             [name (get-field name ast)]
             [args (map (lambda (x) (send x accept this)) (get-field args ast))]
             [signature (send (get-field signature ast) accept this)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast Param%)
        (new Param%
             [var-list (get-field var-list ast)] ;; not copy
             [type (get-field type ast)]
             [known (get-field known ast)]
             [place (get-field place ast)]
             [known-type (get-known-type)]
             [place-type (get-place-type)])]

       [(is-a? ast VarDecl%)
        (new VarDecl%
             [var-list (get-field var-list ast)] ;; not copy
             [type (get-field type ast)]
             [known (get-field known ast)]
             [place (get-field place ast)])]
             
       [(is-a? ast ArrayDecl%)
        (new ArrayDecl%
             [var (get-field var ast)]
             [type (get-field type ast)]
             [known (get-field known ast)]
             [bound (get-field bound ast)]
             [place-list (get-field place-list ast)])]

       [(is-a? ast For%)
        (new For%
             [iter (send (get-field iter ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [from (get-field from ast)]
             [to (get-field to ast)]
             [known (get-field known ast)]
             [place-list (get-field place-list ast)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [(is-a? ast If%)
        (let ([false-block (get-field false-block ast)])
          (new If%
               [condition (send (get-field condition ast) accept this)]
               [true-block (send (get-field true-block ast) accept this)]
               [false-block (and false-block (send false-block accept this))]
               [body-placeset (get-field body-placeset ast)]))] ;; not copy

       [(is-a? ast While%)
        (new While%
             [condition (send (get-field condition ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [bound (get-field bound ast)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [(is-a? ast Assign%)
        (new Assign%
             [lhs (send (get-field lhs ast) accept this)]
             [rhs (send (get-field rhs ast) accept this)])]

       [(is-a? ast Block%)
        (new Block%
             [stmts (map (lambda (x) (send x accept this)) (get-field stmts ast))])]

       [(is-a? ast FuncDecl%)
        (new FuncDecl%
             [name (get-field name ast)]
             [args (send (get-field args ast) accept this)]
             [body (send (get-field body ast) accept this)]
             [return (send (get-field return ast) accept this)]
             [body-placeset (get-field body-placeset ast)])] ;; not copy

       [else
        (raise (format "visitor-cloner: unimplemented for ~a" ast))]

       ))))
             
