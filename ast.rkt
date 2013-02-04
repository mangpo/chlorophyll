#lang racket

(require racket/class)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(require "visitor-interface.rkt")

(provide (all-defined-out))

(define (inc space)
  (string-append space "  "))

(define Base%
  (class object%
    (super-new)
    (init-field [pos #f])   
  ))


(define Livable%
  (class Base%
    (super-new)
    (init-field [place "?"])
    (define/public (get-place)
      place)
    (define/public (set-place new-place)
      (set! place new-place))
  ))

(define Exp%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field [known-type #f])

    (define/public (get-place-known)
      (cons place known-type))

    (define/public (set-place-known x)
      (set! place (car x))
      (set! known-type (cdr x)))

    (define/public (get-known-type)
      known-type)
  ))

(define Num%
  (class Exp%
    (inherit-field known-type place)
    (super-new [known-type #t])
    (when (equal? place "?") (set! place "any"))
    (init-field n)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Num:~a @~a (known=~a))" indent n place known-type)))
    
    (define/public (get-data)
      n)

    (define/public (accept v)
      (send v visit this))
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field known-type place pos)
    (init-field name)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (known=~a))" indent name place known-type)))
    
    (define/public (get-data)
      name)

    (define/public (not-found-error)
      (pretty-display (format "l:~a c:~a error '~a' is undefined." 
                              (position-line pos) 
                              (position-col pos) 
                              name))
      (exit)
      )

    (define/public (accept v)
      (send v visit this))
    ))

(define BinExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field op e1 e2)
    (set! place (get-field place op))

    ;;; Infer place for numbers.
    (when (is-a? e1 Num%) (set-field! place e1 place))
    (when (is-a? e2 Num%) (set-field! place e2 place))
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(BinExp: @~a (known=~a)" indent place known-type))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))
    
    (define/public (accept v)
      (send v visit this))
    ))

(define UnaExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field op e1)
    (set! place (get-field place op))

    ;;; Infer place for numbers.
    (when (is-a? e1 Num%) (set-field! place e1 place))
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp: @~a (known=~a)" indent place known-type))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))
    
    (define/public (accept v)
      (send v visit this))
    ))

(define Op%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field op)
    
    (define/public (add-place new-place)
      (set! place new-place)
      this)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Op:~a @~a)" indent op place)))
    
    (define/public (accept v)
      (send v visit this))
    ))

(define Assign%
  (class Base%
    (super-new)
    (init-field lhs rhs)

    ;;; Infer place for numbers. TODO

    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(ASSIGN" indent))
      (send lhs pretty-print (inc indent))
      (send rhs pretty-print (inc indent))
      )

    (define/public (accept v)
      (send v visit this))
  ))

(define VarDecl%
  (class Exp%
    (super-new)
    (inherit-field place known-type)
    (init-field var type)

    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @~a (known=~a))" indent type var place known-type))
      )

    (define/public (accept v)
      (send v visit this))
  ))

(define Block%
  (class object%
     (super-new)
     (init-field stmts)

     (define/public (pretty-print [indent ""])
       (andmap (lambda (i) (send i pretty-print indent)) stmts))

    (define/public (accept v)
      (send v visit this))
))
