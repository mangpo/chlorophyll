#lang racket

(require racket/class)
(provide (all-defined-out))

(define (inc space)
  (string-append space "  "))

(define Exp%
  (class object%
    (super-new)
    (init-field [known-type #f]
                [place "?"]
                [pos #f])
    (define/public (get-known-type)
      known-type)
    (define/public (get-place)
      place)
    (define/public (set-place new-place)
      (set! place new-place))
  ))

(define Num%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (set! known-type #t)
    (init-field n)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Num:~a @~a (known=~a))" indent n place known-type)))
    
    (define/public (get-data)
      n)
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field name)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (known=~a))" indent name place known-type)))
    
    (define/public (get-data)
      name)
    ))

(define BinExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field op e1 e2)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(BinOp:" indent))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))
    
    ))

(define UnaExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field op e1)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp:" indent))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))
    
    ))

(define Op%
  (class object%
    (super-new)
    (init-field op [place "?"])
    
    (define/public (add-place new-place)
      (set! place new-place)
      this)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Op:~a @~a)" indent op place)))
    
    ))

;(define a (new BinExp% [op 1] [e1 2] [e2 3] [known-type #t]))
;(send a to-string)
;(send a set-place 1)
;(send a to-string)
