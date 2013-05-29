#lang s-exp rosette

(require racket/class)
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(require "header.rkt"
         "visitor-interface.rkt")

(provide (except-out (all-defined-out) inc))

;;;;;;;;;;;;;;;;;;;;;;;;;; Helper Functions ;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-sym)
  (define-symbolic* sym-place number?)
  sym-place)

(define (inc space)
  (string-append space "  "))

(define (at-any? x)
  (or (equal? x #f) (and (is-a? x Place%) (equal? (get-field at x) "any"))))

(define (place-type? p)
  (or (number? p) (place-type-dist? p)))

(define (place-type-dist? p)
  (and (pair? p) (and (and (list? (car p)) (is-a? (cdr p) Base%)))))

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
            (place-list-to-string (car place) out) 
            (send (cdr place) to-string))]

   [else
    (let ([p (evaluate-with-sol place)])
      (if (and out (symbolic? p)) "??" p))]
   ))

;; path-list -> string
(define (path-list-to-string place-list [out #f])
  (foldl (lambda (p str) (string-append (string-append str ", ") 
                                        (send p path-to-string)))
         (send (car place-list) path-to-string) 
         (cdr place-list)))

(define (send-path-to-string path)
  (cond
   [(place-type-dist? path)
    (format "{~a; ~a}" 
            (path-list-to-string (car path)) 
            (send (cdr path) to-string))]

   [(list? path)
    path]

   [else
    (raise (format "send-path-to-string: unimplemented for ~a" path))]))

;; evaluate place
(define (concrete-place place)
  (cond
   [(number? place)
    (evaluate-with-sol place)]

   [(is-a? place Place%) 
    place]

   [(list? place)
    (for ([p place])
	 (send p to-concrete))
    place]

   [(pair? place)
    (cons (concrete-place (car place)) (cdr place))]))
      
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Base%
  (class object%
    (super-new)
    (init-field [pos #f] [send-path #f] [convert #f])   

    (abstract pretty-print)

    (define/public (print-send-path indent)
      (when send-path
            (pretty-display (format "~a(send-path ~a)" (inc indent) 
                                    (send-path-to-string send-path)))))

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

(define Scope%
  (class Base%
    (super-new)
    (init-field [body-placeset #f] [parent #f])

    (define/public (print-body-placeset indent)
      (when body-placeset
            (pretty-display (format "~a(body-placeset ~a)" (inc indent) body-placeset))))))


(define Num%
  (class Exp%
    (inherit-field known-type place-type pos)
    (super-new [known-type #t])
    (init-field n)
    (inherit print-send-path)

    (define/public (get-value)
      (get-field n n))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Num:~a @~a (known=~a))" 
			      indent (get-field n n) (place-to-string place-type) known-type))
      (print-send-path indent))

    (define/public (infer-place [p place-type])
      (when (at-any? place-type)
            (set! place-type p)))

    (define/override (to-string) (send n to-string))
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos)
    (init-field name)
    (inherit print-send-path)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (known=~a))" 
			      indent name (place-to-string place-type) known-type))
      (print-send-path indent))

    (define/override (to-string) name)

    (define/public (not-found-error)
      (raise-syntax-error 'undefined
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))

    (define/public (infer-place [p place-type])
      (when (at-any? place-type)
            (set! place-type p)))

    (define/public (clone)
      (new Var% [name name] [known-type known-type] [place-type place-type] [pos pos]))
    ))

(define Array%
  (class Var%
    (super-new)
    (inherit-field known-type place-type pos name)
    (init-field index)
    (inherit print-send-path)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Array:~a @~a (known=~a))" 
			      indent name (place-to-string place-type) known-type))
      (print-send-path indent)
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
    (inherit print-send-path)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(BinExp: @~a (known=~a)" 
			      indent (place-to-string place-type) known-type))
      (print-send-path indent)
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/public (infer-place [p place-type])
      (when (at-any? place-type)
            (set! place-type p))
      (send e1 infer-place p)
      (send e2 infer-place p))

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
    (inherit print-send-path)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp: @~a (known=~a)" 
			      indent (place-to-string place-type) known-type))
      (print-send-path indent)
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/public (infer-place [p place-type])
      (when (at-any? place-type)
            (set! place-type p))
      (send e1 infer-place p))

    (define/override (to-string)
      (format "(~a ~a)" (send op to-string) (send e1 to-string)))
    
    ))

(define FuncCall%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos)
    (init-field name args [signature #f])
    (inherit print-send-path)

    (define/public (infer-place p)
      void)

    (define/public (copy-at core)
      (new FuncCall% [name name] 
           [args (filter (lambda (x) 
                           (let ([send-path (get-field send-path x)])
                             (or (not send-path) (= (last send-path) core))))
                         args)]
           [known-type known-type]
           [place-type place-type]
           [signature signature]))



    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FuncCall: ~a @~a (known=~a)" 
			      indent name (evaluate-with-sol place-type) known-type))
      (print-send-path indent)
      (for ([arg args])
	   (send arg pretty-print (inc indent)))
      (pretty-display (format "~a)" indent)))

    (define/override (to-string)
      (format "~a(~a)" name (ast-list-to-string args)))

    (define/public (not-found-error)
      (raise-syntax-error 'undefined-function
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))
    ))


(define Const%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field n)
    (inherit get-place print-send-path)

    (define/public (inter-place p)
      (set! place p))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Const:~a @~a)" indent n (get-place)))
      (print-send-path indent))

    (define/public (to-string) n)
))

(define Op%
  (class Livable%
    (super-new)
    (init-field op)
    (inherit get-place print-send-path)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Op:~a @~a)" indent op (get-place)))
      (print-send-path indent))

    (define/public (to-string) op)
    
    ))

(define VarDecl%
  (class Livable%
    (super-new)
    (inherit-field place)
    (init-field var-list type known)
    (inherit get-place print-send-path)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @~a (known=~a))" 
                              indent type var-list place known))
      (print-send-path indent))
  ))

(define Param%
  (class VarDecl%
    (super-new)
    (init-field [place-type #f] [known-type #f])
    (inherit-field var-list)
    
    (define/public (to-string)
      (format "param:~a" (car var-list)))
    
    (define/override (to-concrete)
      (super to-concrete)
      (set! place-type (concrete-place place-type)))))

(define RangePlace%
  (class Livable%
    (super-new)
    (inherit-field place send-path)
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

    (define/public (path-to-string)
      (format "[~a:~a]=~a" from to send-path))
    
    ))

(define For%
  (class Scope%
    (super-new)
    (init-field iter from to body place-list known)
    (inherit print-send-path print-body-placeset)

    (define/public (to-concrete)
      (set! place-list (concrete-place place-list)))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FOR ~a from ~a to ~a) @{~a}" 
			      indent (send iter to-string) from to 
                              (place-to-string place-list)))
      (print-body-placeset indent)
      (print-send-path indent)
      (send body pretty-print (inc indent)))

))

(define ArrayDecl%
  (class LivableGroup%
    (super-new)
    (inherit-field pos place-list)
    (init-field var type known bound)
    (inherit print-send-path)
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(DECL ~a ~a @{~a} (known=~a))" 
                              indent type var
                              (place-to-string place-list)
                              known))
      (print-send-path indent))

    (define/public (bound-error)
      (raise-mismatch-error 'mismatch 
        (format "array boundaries at place annotation of '~a' " var)
	(format "error at src:  l:~a c:~a" (position-line pos) (position-col pos))))

    ))

(define Assign%
  (class Base%
    (super-new)
    (init-field lhs rhs)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ASSIGN" indent))
      (send lhs pretty-print (inc indent))
      (send rhs pretty-print (inc indent))
      )

  ))

(define If%
  (class Scope%
    (super-new)
    (init-field condition true-block [false-block #f])
    (inherit print-send-path)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(IF" indent))
      (print-send-path indent)
      (send condition pretty-print (inc indent))
      (send true-block pretty-print (inc indent))
      (when false-block (send false-block pretty-print (inc indent))))
))

(define While%
  (class Scope%
    (super-new)
    (init-field condition body bound)
    (inherit print-send-path)

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(WHILE" indent))
      (print-send-path indent)
      (send condition pretty-print (inc indent))
      (send body pretty-print (inc indent)))

))

(define Block%
  (class Base%
     (super-new)
     (init-field stmts [parent #f])

     (define/override (pretty-print [indent ""])
       (for ([stmt stmts])
            (send stmt pretty-print indent)))

))

(define FuncDecl%
  (class Scope%
    (super-new)
    (init-field name args body return)
    (inherit-field pos)
    (inherit print-body-placeset)
    ;; args = list of VarDecl%
    ;; return = VarDecl%

    (define/override (pretty-print [indent ""])
      (pretty-display (format "(FUNCTION ~a" name))
      (print-body-placeset indent)
      (when return
	    (send return pretty-print (inc indent)))
      (send args pretty-print (inc indent))
      (send body pretty-print (inc indent)))

    (define/public (not-found-error)
      (raise-syntax-error 'undefined-function
			  (format "'~a' error at src: l:~a c:~a" 
				  name
				  (position-line pos) 
				  (position-col pos))))
    ))

(define Program%
  (class Block%
    (super-new)
    ))

(define Send%
  (class Base%
    (super-new)
    (init-field port)))

(define Recv%
  (class Exp%
    (super-new)
    (init-field data port)))


