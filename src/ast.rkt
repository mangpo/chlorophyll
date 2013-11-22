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

(define (at-io? x)
  (and (is-a? x Place%) (equal? (get-field at x) "io")))

(define (place-type? p)
  (or (number? p) (place-type-dist? p)))

(define (place-type-dist? p)
  (and (pair? p) (and (and (list? (car p)) (is-a? (cdr p) Base%)))))

;; list -> string
(define (list-to-string items [core #f])
  (if (empty? items)
      ""
      (foldl (lambda (item str) 
	       (if core (format "~a, ~a_~a" str item core) (format "~a, ~a" str item)))
	     (if core
		 (format "~a_~a" (car items) core)
		 (format "~a" (car items)))
	     (cdr items))))

;; ast-list -> string
(define (ast-list-to-string ast-list)
  (if (empty? ast-list)
      ""
      (foldl (lambda (ast str) (string-append (string-append str ", ") (send ast to-string))) 
	     (send (car ast-list) to-string) 
	     (cdr ast-list))))

;; place-list -> string
(define (place-list-to-string place-list)
  (foldl (lambda (p str) (string-append (string-append str ", ") (send p to-string))) 
         (send (car place-list) to-string) 
         (cdr place-list)))

;; place-type, place-list -> string
(define (place-to-string place [out #t])
  (cond
   [(is-a? place Place%)
    (send place to-string)]

   [(list? place)
    (format "{~a}" (place-list-to-string place))]

   [(pair? place)
    (format "{~a; ~a}" 
            (place-list-to-string (car place)) 
            (send (cdr place) to-string))]

   [(is-a? place TypeExpansion%)
    (let ([place-list (get-field place-list place)])
      (format "(~a)"
              (foldl (lambda (p str) (format "~a, ~a" str p))
                     (car place-list)
                     (cdr place-list))))]

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

(define (typeexpansion->list place)
  (if (is-a? place TypeExpansion%)
      (for/list ([p (get-field place-list place)])
		(new ProxyReturn% [place-type p]))
      place))

(define (list->typeexpansion lst)
  (new TypeExpansion% [place-list (map (lambda (x) (get-field place-type x)) lst)]))

(define (flatten-arg args)
  (flatten (map (lambda (x) (if (list? (get-field place-type x)) 
                                (get-field place-type x) 
                                x)) 
		args)))

;; evaluate place
(define (concrete-place place)
  ;; (define (all-equal? ref l)
  ;;   (andmap (lambda (x) (= (get-field place x) ref)) l))

  ;; (define (compress p)
  ;;   (let ([ref (get-field place (car p))])
  ;;     (if (all-equal? ref (cdr p))
  ;; 	  ref
  ;; 	  p)))

  (define (compress p)
    (define (compress-inner l)
      (if (empty? (cdr l))
	  l
	  (let ([first (car l)]
		[rest (compress-inner (cdr l))])
	  (if (= (get-field place first) (get-field place (car rest)))
	      (begin
		;; merge
		(set-field! from (car rest) (get-field from first))
		rest)
	      (cons first rest)))))
    
    (let ([ret (compress-inner p)])
      (if (= (length ret) 1)
	  (get-field place (car ret))
	  ret)))

  (cond
   [(number? place)
    (evaluate-with-sol place)]
   
   [(is-a? place Place%) 
    place]

   [(and (list? place) (not (empty? place)) (is-a? (car place) ProxyReturn%))
    (for ([p place])
         (set-field! place-type p (evaluate-with-sol (get-field place-type p))))
    place]
   
   [(list? place)
    (for ([p place])
	 (send p to-concrete))
    (compress place)]
   
   [(pair? place)
    (let ([ret (concrete-place (car place))])
      (if (number? ret)
	  ret
	  (cons ret (cdr place))))]

   [(is-a? place TypeExpansion%)
    (set-field! place-list place 
		(map (lambda (x) (concrete-place x)) (get-field place-list place)))
    place]

   [else
    place]
   ))
    
      
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
   [(or (at-any? place) (at-io? place))
    (set)]
   [(is-a? place TypeExpansion%)
    place]
   [else (raise (format "to-place-set: unimplemented for ~a" place))]))

(define (to-place x)
  (if (place-type-dist? x)
      (car x)
      x))

;; number, place-list -> place-type
(define (to-place-type ast place)
  (cond
   [(or 
     (number? place) 
     (is-a? place Place%)
     (equal? place #f))
    place]

   [(list? place)
    (cons place ast)]
   
   [else (raise (format "to-place-type: unimplemented for ~a" place))]))

(define (filter-place place p)
  (cond
   [(is-a? place TypeExpansion%)
    (new TypeExpansion% [place-list (filter (lambda (x) (= x p)) (get-field place-list place))])]
   [(list? place)
    (filter (lambda (x) (= (get-field place-type x) p)) place)]
   [else
    (raise (format "ast: filter-place: unimplemented for ~a" place))]))
      

(define (get-new-if ast c t f body-placeset [parent #f])
  (let ([constructor (cond
		      [(is-a? ast If!=0%) If!=0%]
		      [(is-a? ast If<0%)  If<0%]
		      [else If%])])
    (new constructor [condition c] [true-block t] [false-block f] [parent parent])))

(define (get-new-while ast c t bound body-placeset pre [parent #f])
  (let ([constructor (cond
		      [(is-a? ast While!=0%) While!=0%]
		      [(is-a? ast While==0%) While==0%]
		      [(is-a? ast While<0%)  While<0%]
		      [(is-a? ast While>=0%) While>=0%]
		      [else While%])])
    (new constructor [condition c] [body t] [parent parent] [pre pre])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define Base%
  (class object%
    (super-new)
    (init-field [pos #f] [send-path #f] [convert #f] [expect 1])   

    (abstract pretty-print clone)

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

;; Place is a representation of place of the form @place(x).
;; Example, @place(any), @place(io), @place(x[i]), @place(valid-expr-in-the-program)
(define Place%
  (class Base%
    (super-new)
    (init-field at)
    (inherit-field pos)

    (define/override (clone)
      (new Place% [at (if (is-a? at Base%) (send at clone) at)]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Place:~a)" indent (if (is-a? at Base%)
							(send at to-string)
							at))))
    
    (define/public (to-string)
      (if (equal? at "any")
	  "any"
	  (format "place(~a)" (if (is-a? at Base%) (send at to-string) at)))
      )

    (define/public (illegal-place)
      (define string (to-string))
      (raise (format "~a is illegal. It is clusterd array. Error at src: l:~a c:~a"
		     string (position-line pos) (position-col pos))))
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
    (init-field [known-type #f] [place-type #f] [cluster #f] [expand 1] [type #f])

    (define/public (infer-place p)
      ;(pretty-display `(infer-place ,p ,place-type))
      (when (and p (at-any? place-type))
            (set! place-type p)))

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
    (init-field [body-placeset (set)] [parent #f])

    (define/override (clone)
      (new Scope%))

    (define/public (print-body-placeset indent)
      (when body-placeset
            (pretty-display (format "~a(body-placeset ~a)" (inc indent) body-placeset))))))


(define Num%
  (class Exp%
    (inherit-field known-type place-type pos expect expand)
    (super-new [known-type #t] [type "int"] [expand 1])
    (init-field n)
    (inherit print-send-path)

    (define/override (clone)
      (new Num% [n (send n clone)] [pos pos]))

    (define/public (get-value)
      (get-field n n))
    
    (define/override (pretty-print [indent ""])
      ;; (pretty-display (format "~a(Num:~a @~a (known=~a))" 
      ;;   		      indent (get-field n n) (place-to-string place-type) known-type))
      (pretty-display (format "~a(Num:~a @~a @~a (expand=~a/~a))" 
			      indent (get-field n n) place-type
                              (get-field place n)
                              expand expect))
      (print-send-path indent))

    (define/override (to-string) (send n to-string))
    ))

(define Var%
  (class Exp%
    (super-new)
    (inherit-field type known-type place-type pos expand expect)
    (init-field name [sub #f] [address #f] [compact #f])
    (inherit print-send-path)
    
    (define/override (clone)
      (new Var% [name name] [known-type known-type] [place-type place-type] [pos pos]
           [expand expand] [expect expect] [type type] [compact compact] [sub sub]))
    
    (define/public (clone-at p)
      (new Var% [name name] [known-type known-type] 
	   [place-type (filter-place place-type p)] 
	   [pos pos]
           [expand expand] [expect expect] [type type] [compact compact] [sub sub]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Var:~a @~a (expand=~a/~a) (type=~a) (compact=~a))" 
                              indent name place-type
                              expand expect type compact))
      (print-send-path indent))

    (define/override (to-string) name)

    (define/public (not-found-error)
      (if pos
	  (raise-syntax-error 'undefined
			      (format "'~a' error at src: l:~a c:~a" 
				      name (position-line pos) (position-col pos)))
	  (raise-syntax-error 'undefined (format "'~a'" name))))

    (define/public (partition-mismatch part expect)
      (raise-mismatch-error 'data-partition
			    (format "number of data partitions at '~a' is ~a, expect <= ~a" 
				    name part expect)
			    (format "error at src  l:~a c:~a" (position-line pos) (position-col pos))))
    ))

(define Temp%
  (class Var%
    (super-new)
    (init-field [link #f] [decl #f] [eqv #f])
    (inherit-field name place-type known-type pos expand expect type compact sub)
    
    (define/override (clone)
      ;; don't copy link & decl
      (new Temp% [name name] [known-type known-type] [place-type place-type] [pos pos]
           [expand expand] [expect expect] [type type] [compact compact] [sub sub]
           [eqv eqv]))

    (define/override (infer-place p)
      (when (and p (at-any? place-type))
            (set! place-type p)
            (when link
                  (send link infer-place p))
            (when decl
                  (send decl infer-place (to-place p)))))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Temp:~a place-type:~a compact:~a sub:~a)" indent name 
                              (place-to-string place-type) compact sub)))))

(define Array%
  (class Var%
    (super-new)
    (inherit-field known-type place-type pos name expand expect type)
    (init-field index [offset 0] [opt #f])
    (inherit print-send-path)

    (define/override (clone)
      (new Array% [name name] [index (send index clone)] [offset offset] [type type]
           [known-type known-type] [place-type place-type] [pos pos]))

    (define/override (pretty-print [indent ""])
      ;; (pretty-display (format "~a(Array:~a @~a (known=~a))" 
      ;;   		      indent name (place-to-string place-type) known-type))
      (pretty-display (format "~a(Array:~a @~a (expand=~a/~a))" 
			      indent name (place-to-string place-type)
                              expand expect))
      (print-send-path indent)
      (when (> offset 0)
	    (pretty-display (format "~a(offset: ~a)" (inc indent) offset)))
      (send index pretty-print (inc indent)))

    (define/override (to-string)
      (if (> offset 0)
	  (format "~a[(~a)-~a]" name (send index to-string) offset)
	  (format "~a[~a]" name (send index to-string)))
      )

    (define/public (index-out-of-bound index)
      (raise-range-error 'array "error at src" "" index 
			 (format "l:~a c:~a" (position-line pos) (position-col pos))
			 0 3))
    ))

;; AST for Binary opteration. 
(define BinExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos type)
    (init-field op e1 e2)
    (inherit print-send-path)
        
    (define/override (clone)
      (new BinExp% [op (send op clone)] [e1 (send e1 clone)] [e2 (send e2 clone)]
	   [known-type known-type] [place-type place-type] [pos pos] [type type]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(BinExp: @~a (known=~a)" 
			      indent place-type known-type))
      (print-send-path indent)
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (send e2 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/override (infer-place p)
      (when (and p (at-any? place-type))
            (set! place-type p)
            (send e1 infer-place p)
            (send e2 infer-place p)))

    (define/override (to-string)
      (format "(~a ~a ~a)" (send e1 to-string) (send op to-string) (send e2 to-string)))

    ))

;; AST for Unary opteration. 
(define UnaExp%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos type)
    (init-field op e1)
    (inherit print-send-path)

    (define/override (clone)
      (new UnaExp% [op (send op clone)] [e1 (send e1 clone)] [known-type known-type] 
	   [place-type place-type] [pos pos] [type type]))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(UnaOp: @~a (known=~a)" 
			      indent place-type known-type))
      (print-send-path indent)
      (send op pretty-print (inc indent))
      (send e1 pretty-print (inc indent))
      (pretty-display (format "~a)" indent)))

    (define/override (infer-place p)
      (when (and p (at-any? place-type))
            (set! place-type p)
            (send e1 infer-place p)))

    (define/override (to-string)
      (format "(~a ~a)" (send op to-string) (send e1 to-string)))
    
    ))


(define FuncCall%
  (class Exp%
    (super-new)
    (inherit-field known-type place-type pos expand expect type)
    (init-field name args [signature #f] [is-stmt #f] [might-need-storage #f])
    (inherit print-send-path)

    (define/override (clone)
      (new FuncCall% [name name] [args (map (lambda (x) (send x clone)) args)]
           [place-type place-type] ;[signature (send signature get-signature)]
           [type type]))

    (define/public (copy-at core)
      (new FuncCall% [name name] 
           [args (filter (lambda (x) 
                           (let ([send-path (get-field send-path x)])
                             (or (not send-path) (= (last send-path) core))))
                         args)]
           [known-type known-type]
           [place-type place-type]
           [signature signature]
           [type type]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FuncCall: ~a @~a (expand=~a/~a) (type=~a)" 
			      indent name (evaluate-with-sol place-type)
                              expand expect type))
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

    (define/public (type-error)
      (raise-mismatch-error 'function-type
			    (format "function ~a is void (can't be used in an expression)"
                                    name)
			    (format "error at src  l:~a c:~a" 
                                    (position-line pos) (position-col pos))))

    (define/public (partition-mismatch part expect)
      (raise-mismatch-error 'data-partition
			    (format "number of data partitions at '~a' is ~a, expect <= ~a" 
				    name part expect)
			    (format "error at src  l:~a c:~a" 
                                    (position-line pos) (position-col pos))))
  
    (define/public (args-mismatch l)
      (raise-mismatch-error 'mismatch
			    (format "function ~a expects ~a arguments, but ~a arguments are given\n"
				    name l (length args))
			    (format "error at src  l:~a c:~a" (position-line pos) (position-col pos))))
    ))

(define FuncCallDup%
  (class FuncCall%
    (super-new)
    (inherit-field place-type type name args)

    (define/override (clone)
      (new FuncCallDup% [name name] [args (map (lambda (x) (send x clone)) args)]
           [place-type place-type] ;[signature (send signature get-signature)]
           [type type]))))

(define Const%
  (class Livable%
    (super-new)
    (inherit-field place pos)
    (init-field n)
    (inherit get-place print-send-path)
    (set! place #f)

    (define/override (clone)
      (new Const% [n n] [place place] [pos pos]))

    (define/public (inter-place p)
      (set! place p))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Const:~a @~a)" indent n (get-place)))
      (print-send-path indent))

    (define/public (to-string) (number->string n))

))

(define Op%
  (class Livable%
    (super-new)
    (init-field op)
    (inherit-field pos)
    (inherit get-place print-send-path)

    (define/override (clone)
      (new Op% [op op] [pos pos]))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Op:~a @~a)" indent op (get-place)))
      (print-send-path indent))

    (define/public (to-string) op)
    
    ))

(define VarDecl%
  (class Livable%
    (super-new)
    (inherit-field place pos)
    (init-field var-list type [known #t] [address #f] [compact #f])
    (inherit get-place print-send-path)

    (define/public (infer-place p)
      (when (and p (at-any? place))
            (set! place p)))

    (define/override (clone)
      (new VarDecl% [var-list var-list] [type type] [known known] [place place]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(VARDECL ~a ~a @~a (address=~a))" 
                              indent type var-list place address))
      (print-send-path indent))

    (define/public (partition-mismatch)
      (raise-mismatch-error 'data-partition
			    (format "number of data partitions and places at '~a'" var-list)
			    (format "error at src  l:~a c:~a" (position-line pos) (position-col pos))))
  ))

(define ReturnDecl%
  (class VarDecl%
    (super-new)
    (inherit-field compact var-list type known place)
    (set! compact #t)

    (define/override (clone)
      (new ReturnDecl% [var-list var-list] [type type] [known known] [place place]))
      
    ))

(define TempDecl%
  (class VarDecl%
    (super-new)
    (inherit-field var-list type known place)

    (define/override (clone)
      (new TempDecl% [var-list var-list] [type type] [known known] [place place]))
    ))

(define Param%
  (class VarDecl%
    (super-new)
    (init-field [place-type #f] [known-type #t])
    (inherit-field var-list type known place)

    (define/override (clone)
      (new Param% [var-list var-list] [type type] 
           [known known] [place place]
           [known-type known-type] [place-type place-type]))

    (define/public (set-known val)
      (set! known val)
      (set! known-type val))

    (define/override (infer-place p)
      (when (and p (at-any? place-type))
            (set! place p)
            (set! place-type p)))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(PARAM ~a ~a @~a (place-type=~a)" 
                              indent type var-list place place-type)))

    (define/public (to-string)
      (format "param:~a" (car var-list)))
    
    (define/override (to-concrete)
      (super to-concrete)
      (set! place-type (concrete-place place-type))
      (set! place (concrete-place place)))))

(define RangePlace%
  (class Livable%
    (super-new)
    (inherit-field place send-path)
    (init-field from to)
    (inherit get-place)

    (define/override (clone)
      (new RangePlace% [from from] [to to] [place (if (is-a? place Base%) (send place clone) place)]))

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

;; TypeExpansion represents place of packed variables.
;; For example place of int::2@(0,1) x; int::3[] y[10];
;; Element in place-list can be
;; 1) number/symbolic
;; 2) list of RangePlace%
;; 3) (cons list of RangPlace% . exp) 
(define TypeExpansion%
  (class Base%
    (super-new)
    (init-field place-list)

    (define/override (clone)
      (new TypeExpansion% [place-list place-list]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(Place-type-expansion ~a)" place-list)))))

(define ProxyReturn%
  (class Exp%
    (super-new)
    (inherit-field place-type)

    (define/override (clone)
      (new ProxyReturn% [place-type place-type]))

    (define/override (to-string) (format "~a" place-type))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ProxyReturn @~a)" place-type)))
    ))

(define For%
  (class Scope%
    (super-new)
    (init-field iter from to body [known #t] 
		[place-list (new Place% [at "any"])] [address #f] [iter-type 0]
                [unroll #f])
    (inherit print-send-path print-body-placeset)

    (define/override (clone)
      (new For% [iter iter] [from from] [to to] [place-list place-list] 
           [body (send body clone)] [known known] [unroll unroll]))

    (define/public (to-concrete)
      (set! place-list (concrete-place place-list)))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(FOR ~a from ~a to ~a) @{~a}" 
			      indent (send iter to-string) from to 
                              (place-to-string place-list)))
      (print-body-placeset indent)
      (print-send-path indent)
      (when unroll
            (pretty-display (format "~a(unroll: ~a)" (inc indent) unroll)))
      (send body pretty-print (inc indent)))

))

(define ArrayDecl%
  (class LivableGroup%
    (super-new)
    (inherit-field pos place-list)
    (init-field var type bound cluster init [known #t] [compress (min 2 bound)] 
                [address #f] [offset 0])
    (inherit print-send-path)

    (define/override (clone)
      (new ArrayDecl% [var var] [type type] [bound bound] [cluster cluster] [init init]
           [place-list place-list]))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ARRAYDECL ~a ~a @{~a} (known=~a) (cluster=~a) (compress=~a) " 
                              indent type var
			      place-list
                              ;(place-to-string place-list)
                              known cluster compress))
      (pretty-display (format "~a(init ~a)" (inc indent) init))
      (print-send-path indent))

    (define/public (bound-error)
      (raise-mismatch-error 'mismatch 
        (format "array boundaries at place annotation of '~a' " var)
	(format "error at src:  l:~a c:~a" (position-line pos) (position-col pos))))

    (define/public (init-mismatch)
      (raise-mismatch-error 'entry-mismatch
        (format "initial value entries doesn't match the declaration at array '~a' " var)
        (format "error at src: l:~a c:~a" (position-line pos) (position-col pos))))

    (define/public (update-compress lowerbound)
      (when (>= lowerbound compress)
            (set! compress (min (add1 lowerbound) bound))))

    ))

(define Assign%
  (class Base%
    (super-new)
    (init-field lhs rhs)

    (define/override (clone)
      (new Assign% [lhs (send lhs clone)] [rhs (send rhs clone)]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ASSIGN" indent))
      (send lhs pretty-print (inc indent))
      (send rhs pretty-print (inc indent))
      )
  ))

(define AssignTemp%
  (class Assign%
    (super-new)
    (inherit-field lhs rhs)
    (init-field [info #f])

    (define/override (clone)
      (new AssignTemp% [lhs (send lhs clone)] [rhs (send rhs clone)]))

    (define/public (infer-place p)
      (when p
            (send lhs infer-place p)
            (send rhs infer-place p)))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(ASSIGNTEMP" indent))
      (send lhs pretty-print (inc indent))
      (send rhs pretty-print (inc indent))
      )
  ))

(define Return%
  (class Base%
    (super-new)
    (init-field val)

    (define/override (clone)
      (new Return% [val (if (list? val) 
                            (map (lambda (x) (send x clone)) val)
                            (send val clone))]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(RETURN" indent))
      (if (list? val)
          (for ([x val])
               (send x pretty-print (inc indent)))
          (send val pretty-print (inc indent))))
    ))

(define If%
  (class Scope%
    (super-new)
    (init-field condition true-block [false-block #f] [pre #f])
    (inherit print-send-path)

    (define/override (clone)
      (new If% [condition (send condition clone)] 
           [true-block (send true-block clone)]
           [false-block (and false-block (send false-block clone))]
           [pre (and pre (send pre clone))]))

    (define/public (pretty-print-content indent)
      (print-send-path indent)
      (send condition pretty-print (inc indent))
      (send true-block pretty-print (inc indent))
      (when false-block (send false-block pretty-print (inc indent))))
    
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(IF" indent))
      (pretty-print-content indent))
))

;; Corresponds to 'if' in arrayforth.
(define If!=0%
  (class If%
    (super-new)
    (inherit-field condition true-block false-block pre)
    (inherit pretty-print-content)
    
    (define/override (clone)
      (new If!=0% [condition (send condition clone)] 
           [true-block (send true-block clone)]
           [false-block (and false-block (send false-block clone))]
           [pre (and pre (send pre clone))]))
      
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(IF!=0" indent))
      (pretty-print-content indent))
    ))

;; Correspond to '-if' in arrayforth.
(define If<0%
  (class If%
    (super-new)
    (inherit-field condition true-block false-block pre)
    (inherit pretty-print-content)
    
    (define/override (clone)
      (new If<0% [condition (send condition clone)] 
           [true-block (send true-block clone)]
           [false-block (and false-block (send false-block clone))]
           [pre (and pre (send pre clone))]))
      
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(IF<0" indent))
      (pretty-print-content indent))
    ))

(define While%
  (class Scope%
    (super-new)
    (init-field condition body [bound 100] [pre (new Block% [stmts (list)])])
    (inherit print-send-path)

    (define/override (clone)
      (new While% [condition (send condition clone)] [body (send body clone)]
           [pre (send pre clone)] [bound bound]))

    (define/public (pretty-print-content indent)
      (print-send-path indent)
      (pretty-display (format "~a>>> pre" (inc indent)))
      (send pre pretty-print (inc indent))
      (pretty-display (format "~a>>> cond" (inc indent)))
      (send condition pretty-print (inc indent))
      (pretty-display (format "~a>>> body" (inc indent)))
      (send body pretty-print (inc indent)))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(WHILE" indent))
      (pretty-print-content indent))

))

;; Correspond to recursion with 'if'.
(define While!=0%
  (class While%
   (super-new)
   (inherit-field condition body bound pre)
   (inherit pretty-print-content)

   (define/override (clone)
     (new While!=0% [condition (send condition clone)] [body (send body clone)]
          [pre (send pre clone)] [bound bound]))

   (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(While!=0" indent))
      (pretty-print-content indent))
))

;; Correspond to recursion with reversed-true-false 'if'.
(define While==0%
  (class While%
   (super-new)
   (inherit-field condition body bound pre)
   (inherit pretty-print-content)

   (define/override (clone)
     (new While==0% [condition (send condition clone)] [body (send body clone)]
          [pre (send pre clone)] [bound bound]))

   (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(While==0" indent))
      (pretty-print-content indent))
))

;; Correspond to recursion with '-if'.
(define While<0%
  (class While%
   (super-new)
   (inherit-field condition body bound pre)
   (inherit pretty-print-content)

   (define/override (clone)
     (new While<0% [condition (send condition clone)] [body (send body clone)]
          [pre (send pre clone)] [bound bound]))

   (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(While<0" indent))
      (pretty-print-content indent))
))

;; Correspond to recursion with reversed-true-false 'iif'.
(define While>=0%
  (class While%
   (super-new)
   (inherit-field condition body bound pre)
   (inherit pretty-print-content)

   (define/override (clone)
     (new While>=0% [condition (send condition clone)] [body (send body clone)]
          [pre (send pre clone)] [bound bound]))

   (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(While>=0" indent))
      (pretty-print-content indent))
))

(define Block%
  (class Base%
     (super-new)
     (init-field stmts [parent #f])

     (define/override (clone)
       (new Block% [stmts (map (lambda (x) (send x clone)) stmts)]))

     (define/override (pretty-print [indent ""])
       (for ([stmt stmts])
            (send stmt pretty-print indent)))

))

(define Program%
  (class Block%
    (super-new)
    (inherit-field stmts)

    (define/override (clone)
      (new Program% [stmts (map (lambda (x) (send x clone)) stmts)]))
    ))

(define FuncDecl%
  (class Scope%
    (super-new)
    (init-field name args body return [temps (list)] [regs 0])
    (inherit-field pos body-placeset)
    (inherit print-body-placeset)
    ;; args = list of VarDecl%
    ;; return = VarDecl%

    (define/override (clone)
      (new FuncDecl% [name name] [args (send args clone)]
           [return (and return (send return clone))]
           [body (send body clone)] [temps temps]))

    (define/public (get-signature)
       (new FuncDecl% [name name] [args (send args clone)]
           [return (and return (send return clone))]
           [body #f]))

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

(define Send%
  (class Base%
    (super-new)
    (init-field data port)

    (define/override (clone)
      (new Send% [data (send data clone)] [port port]))
    
    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(SEND to:~a" indent port))
      (send data pretty-print (inc indent)))))

(define Recv%
  (class Exp%
    (super-new)
    (init-field port)

    (define/override (clone)
      (new Recv% [port port]))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(RECV from:~a)" indent port)))

    (define/override (to-string)
      (format "read(~a)" port))))

(define Range%
  (class Base%
    (super-new)
    (init-field from to)

    (define/override (clone)
      (raise "no clone for Range%"))

    (define/override (pretty-print [indent ""])
      (pretty-display (format "~a(RANGE ~a ~a)" indent from to)))

    (define/public (to-string)
      (format "[~a,~a]" from to))
    ))
    


