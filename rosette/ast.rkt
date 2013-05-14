#lang s-exp rosette

(require racket/class)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(require "header.rkt"
         "visitor-interface.rkt")

(provide (except-out (all-defined-out) inc))

(define (get-sym)
  (define-symbolic* sym-place number?)
  sym-place)

(define (inc space)
  (string-append space "  "))

(define Base%
  (class object%
    (super-new)
    (init-field [pos #f])   

    (abstract pretty-print)

    (define/public (accept v)
      (send v visit this))

    (define/public (get-line)
      (position-line pos))

    (define/public (get-col)
      (position-col pos))
    ))

(define Place%
  (class Base%
    (super-new)
    (init-field at)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Place:~a)" indent (if (is-a? at Base%)
							(send at to-string)
							at))))
    
    (define/public (to-string)
      (if (equal? at "any")
	  "any"
	  (format "place(~a)" (if (is-a? at Base%) (send at to-string) at)))
      )

    ))

(define Livable%
  (class Base%
    (super-new)
    (init-field [place (get-sym)])

    (define/public (get-place)
      (evaluate-with-sol place))
    (define/public (set-place new-place)
      (set! place new-place))

    (define/public (to-concrete)
      (set! place (concrete-place place)))
    ))


;; list -> string
(define (list-to-string items)
  (if (empty? items)
      ""
      (foldl (lambda (item str) (format "~a, ~a" item str))
	     (format "~a" (car items))
	     (cdr items))))

;; ast-list -> string
(define (ast-list-to-string ast-list)
  (if (empty? ast-list)
      ""
      (foldl (lambda (ast str) (string-append (string-append str ", ") (send ast to-string))) 
	     (send (car ast-list) to-string) 
	     (cdr ast-list))))

;; place-list -> string
(define (place-list-to-string place-list [out #f])
  (foldl (lambda (p str) (string-append (string-append str ", ") (send p to-string out))) 
         (send (car place-list) to-string out) 
         (cdr place-list)))

;; place-type, place-list -> string
(define (place-to-string place [out #t])
  (cond
   [(is-a? place Place%)
    (send place to-string)]

   [(list? place)
    (format "{~a}" (place-list-to-string place out))]

   [(pair? place)
    (format "{~a; ~a}" 
            (place-to-string (car place)) 
            (send (cdr place) to-string))]

   [else
    (let ([p (evaluate-with-sol place)])
      (if (and out (symbolic? p)) "??" p))]
   ))

;; evaluate place
(define (concrete-place place)
  (cond
   [(number? place)
    (evaluate-with-sol place)]

   [(is-a? place Place%) 
    place]

   [(list? place)
    (for ([p place])
	 (send p to-concrete)
	 place)]

   [(pair? place)
    (concrete-place (car place))]))
      
;; number, place-list, place-type -> set
(define (to-place-set place)
  (cond
   [(number? place)
    (set place)]
   [(list? place)
    (foldl (lambda (p place-set) (set-add place-set (get-field place p)))
           (set) place)]
   [(pair? place)
    (to-place-set (car place))]
   [(and (is-a? place Place%) (equal? (get-field at place) "any"))
    (set)]
   [else (raise "Error: cannot handle this object type")]))

;; number, place-list -> place-type
(define (to-place-type ast place)
  (if (or (number? place) (is-a? place Place%))
      place
      (cons place ast)))

(define LivableGroup%
  (class Base%
    (super-new)
    (init-field place-list) ; doesn't have to be list

    (define/public (to-concrete)
      (set! place-list (concrete-place place-list)))
))

(define Exp%
  (class Base%
    (super-new)
    (init-field [known-type #f] [place-type #f])

    (define/public (get-place-known)
      (cons place-type known-type))

    (define/public (set-place-known x)
      (set! place-type (car x))
      (set! known-type (cdr x)))

    (define/public (get-known-type)
      known-type)

    (define/public (get-place)
      (place-to-string place-type))

    (define/public (to-concrete)
      (set! place-type (concrete-place place-type)))

    ;; This is used to construct place-type representation.
    (abstract to-string)
  ))


(define Num%
  (class Exp%
    (inherit-field known-type place-type pos)
    (super-new [known-type #t])
    (init-field n)
    (set! place-type (get-field place n))

    (define/public (infer-place p)
      ;(set-field! place n p)
      (set! place-type p))

    (define/public (get-value)
      (get-field n n))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Num:~a @~a (known=~a))" 
			      indent (get-field n n) (place-to-string place-type) known-type)))

    (define/override (to-string) (send n to-string))
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos)
    (init-field name)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (known=~a))" 
			      indent name (place-to-string place-type) known-type)))

    (define/override (to-string) name)

    (define/public (not-found-error)
      (raise-syntax-error 'undefined
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))
    ))

(define Array%
  (class Var%
    (super-new)
    (inherit-field known-type place-type pos name)
    (init-field index)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Array:~a @~a (known=~a))" 
			      indent name (place-to-string place-type) known-type))
      (send index pretty-print (inc indent)))

    (define/override (to-string)
      (format "~a[~a]" name (send index to-string)))

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
    (inherit-field known-type place-type)
    (init-field op e1 e2)
    (set! place-type (get-field place op))

    ;;; Infer place for numbers.
    (when (is-a? e1 Num%) (send e1 infer-place place-type))
    (when (is-a? e2 Num%) (send e2 infer-place place-type))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(BinExp: @~a (known=~a)" 
			      indent (place-to-string place-type) known-type))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/override (to-string)
      (format "(~a ~a ~a)" (send e1 to-string) (send op to-string) (send e2 to-string)))
    
    ))

;; AST for Binary opteration. Easy inferences happen here.
;; If the operand is a constant, infer its placement equal to the operator's.
(define UnaExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type)
    (init-field op e1)
    (set! place-type (get-field place op))

    ;;; Infer place for numbers.
    (when (is-a? e1 Num%) (send e1 infer-place place-type))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp: @~a (known=~a)" 
			      indent (place-to-string place-type) known-type))
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/override (to-string)
      (format "(~a ~a)" (send op to-string) (send e1 to-string)))
    
    ))

(define FuncCall%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type)
    (init-field name args)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FuncCall: ~a @~a (known=~a)" 
			      indent name (evaluate-with-sol place-type) known-type))
      (for ([arg args])
	   (send arg pretty-print (inc indent)))
      (pretty-display (format "~a)" indent)))

    (define/override (to-string)
      (format "~a(~a)" name (ast-list-to-string args)))
    ))


(define Const%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field n)
    (inherit get-place)

    (define/public (inter-place p)
      (set! place p))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Const:~a @~a)" indent n (get-place))))

    (define/public (to-string) n)
))

(define Op%
  (class Livable%
    (super-new)
    (init-field op)
    (inherit get-place)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Op:~a @~a)" indent op (get-place))))

    (define/public (to-string) op)
    
    ))

(define VarDecl%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field var-list type known)
    (inherit get-place)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @~a (known=~a))" 
                              indent type var-list place known)))
  ))

(define Param%
  (class VarDecl%
    (super-new)
    (init-field [place-type #f] [known-type #f])
    
    (define/override (to-concrete)
      (super to-concrete)
      (set! place-type (concrete-place place-type)))))

(define RangePlace%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field from to)
    (inherit get-place)

    (define/override (pretty-print)
      (pretty-display (to-string)))

    (define/public (equal-rangeplace? other)
      (and (and (equal? from (get-field from other))
                (equal? to   (get-field to   other)))
           (equal? place (get-field place other))))
    
    (define/public (to-string [out #f])
      (let* ([place (get-place)]
	     [print (if (and out (symbolic? place)) "??" place)])
	(format "[~a:~a]=~a" from to print)))
    
    ))

(define For%
  (class LivableGroup%
    (super-new)
    (init-field iter from to body known)
    (inherit-field place-list)
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FOR ~a from ~a to ~a) @{~a}" 
			      indent (send iter to-string) from to 
                              (place-to-string place-list)))
      (send body pretty-print (inc indent)))

))

(define ArrayDecl%
  (class LivableGroup%
    (super-new)
    (inherit-field pos place-list)
    (init-field var type known bound)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @{~a} (known=~a))" 
                              indent type var
                              (place-to-string place-list)
                              known)))

    (define/public (bound-error)
      (raise-mismatch-error 'mismatch 
        (format "array boundaries at place annotation of '~a' " var)
	(format "error at src:  l:~a c:~a" (position-line pos) (position-col pos))))

    ))

(define Assign%
  (class Base%
    (super-new)
    (init-field lhs rhs)

    ;;; Infer place for numbers. TODO

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ASSIGN" indent))
      (send lhs pretty-print (inc indent))
      (send rhs pretty-print (inc indent))
      )

  ))

(define If%
  (class Base%
    (super-new)
    (init-field condition true-block [false-block #f])

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(IF" indent))
      (send condition pretty-print (inc indent))
      (send true-block pretty-print (inc indent))
      (when false-block (send false-block pretty-print (inc indent))))
))

(define While%
  (class Base%
    (super-new)
    (init-field condition body bound)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(WHILE" indent))
      (send condition pretty-print (inc indent))
      (send body pretty-print (inc indent)))

))

(define Block%
  (class Base%
     (super-new)
     (init-field stmts [firstexp #f])

     (define/override (pretty-print [indent ""])
       (for ([stmt stmts])
            (send stmt pretty-print indent)))

))

(define FuncDecl%
  (class Base%
    (super-new)
    (init-field name args body return)
    (inherit-field pos)
    ;; args = list of VarDecl%
    ;; return = VarDecl%

    (define/override (pretty-print [indent ""])
      (pretty-display (format "(FUNCTION ~a" name))
      (when return
	    (send return pretty-print (inc indent)))
      (send args pretty-print (inc indent))
      (send body pretty-print (inc indent)))

    (define/public (not-found-error)
      (raise-syntax-error 'undefined
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))
    ))

(define Program%
  (class Base%
    (super-new)
    (init-field decls)

    (define/override (pretty-print [indent ""])
      (for ([decl decls])
           (send decl pretty-print)))

    ))


