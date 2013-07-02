#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" 
         "ast-util.rkt"
         "parser.rkt"
         "visitor-interface.rkt")

(provide work-desugarer%)

(define work-desugarer%
  (class* object% (visitor<%>)
    (super-new)
    
    (define/public (visit ast)
      (cond
       [(is-a? ast Program%)
        (define stmts (get-field stmts ast))
        (for ([stmt stmts] #:when (is-a? stmt AbstractFilterDecl%))
          (send stmt accept this))]

       [(is-a? ast AbstractFilterDecl%)
        (define body (get-field body ast))
        (set-field! stmts body
                    (for/list ([stmt (get-field stmts body)])
                      (if (is-a? stmt WorkBlocking%)
                          (new While%
                               [condition (new Num% [n (new Const% [n 1] [pos (get-field pos stmt)])])]
                               [body (get-field block stmt)]
                               [pos (get-field pos stmt)])
                          stmt)))]

       [else (raise (format "visitor-workdesugar: unimplemented for ~a" ast))]))
))
