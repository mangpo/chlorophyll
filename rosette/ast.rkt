#lang s-exp rosette

(require racket/class)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(require "header.rkt"
         "visitor-interface.rkt")

(provide (all-defined-out))

(define (get-sym)
  (define-symbolic* sym-place number?)
  sym-place)

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
    (init-field [place (get-sym)])

    (define/public (get-place)
      (evaluate place))
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
    (inherit-field known-type place pos)
    (super-new [known-type #t])
    ;(when (symbolic? place) (set! place 0)) ; place = any
    (init-field n)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Num:~a @~a (known=~a))" indent n (evaluate place) known-type)))
    
    (define/public (get-data)
      n)

    (define/public (accept v)
      (send v visit this))

    (define/public (hash-code) n)
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field known-type place pos)
    (init-field name)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (known=~a))" indent name (evaluate place) known-type)))
    
    (define/public (get-data)
      name)

    (define/public (not-found-error)
      (raise-syntax-error 'undefined
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))

    (define/public (accept v)
      (send v visit this))
    ))

(define Array%
  (class Var%
    (super-new)
    (inherit-field known-type place pos name)
    (init-field index)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Array:~a @~a (known=~a))" indent name (evaluate place) known-type))
      (send index pretty-print (inc indent)))

    (define/public (index-out-of-bound index)
      (raise-range-error 'array "error at src" "" index 
			 (format "l:~a c:~a" (position-line pos) (position-col pos))
			 0 3))


    ))

;; AST for Binary opteration. Easy inferences happen here.
;; If left or right operand is a constant, infer its placement equal to the operator's.
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
      (pretty-display (format "~a(BinExp: @~a (known=~a)" indent (evaluate place) known-type))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))
    
    (define/public (accept v)
      (send v visit this))
    ))

;; AST for Binary opteration. Easy inferences happen here.
;; If the operand is a constant, infer its placement equal to the operator's.
(define UnaExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place)
    (init-field op e1)
    (set! place (get-field place op))

    ;;; Infer place for numbers.
    (when (is-a? e1 Num%) (set-field! place e1 place))
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp: @~a (known=~a)" indent (evaluate place) known-type))
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
      (pretty-display (format "~a(Op:~a @~a)" indent op (evaluate place))))
    
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
    (init-field var-list type)

    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @~a (known=~a))" 
                              indent type var-list (evaluate place) known-type))
      
      )

    (define/public (accept v)
      (send v visit this))
  ))

(define ArrayDecl%
  (class Exp%
    (super-new)
    (inherit-field place known-type pos)
    (init-field var type bound)
    
    (define/public (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @{~a} (known=~a))" 
                              indent type var
                              (send place to-string)
                              known-type)))

    (define/public (bound-error)
      (raise-mismatch-error 'mismatch 
        (format "array boundaries at place annotation of '~a' " var)
	(format "error at src:  l:~a c:~a" (position-line pos) (position-col pos))))

    (define/public (accept v)
      (send v visit this))
    ))

(define RangePlaceList%
  (class object%
    (super-new)
    (init-field place-list)
    
    (define/public (to-string)
      (foldl (lambda (p str) (string-append (string-append str ", ") (send p to-string))) 
             (send (car place-list) to-string) (cdr place-list)))
))

(define RangePlace%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field from to)
    
    (define/public (to-string)
      (format "[~a:~a]=~a" from to (evaluate place)))
    
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

(define For%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field iter from to block)

    (define/public (pretty-print [indent ""])
      (display (format "~a(FOR ~a from ~a to ~a) @{" iter from to))
      (display
       (if (number? place)
           place
           (send place to-string)))
      (pretty-display "}")
      (send block pretty-print (inc indent)))

))
