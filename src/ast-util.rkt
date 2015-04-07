#lang s-exp rosette

(require "header.rkt" "ast.rkt")

(provide (all-defined-out))

(define (ext-name name ext)
  (format "~a::~a" name ext))

(define (get-stdin)
  (define stdin
    (new FuncDecl% [name "in"] 
	 [args (new Block% [stmts (list)])] 
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (new Place% [at "io"]))] 
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "int"] ;; TODO: make it generic
		      [place (new Place% [at "io"])]
		      [known #f])]))
  stdin)

(define (get-stdout)
  (define stdout
    (new FuncDecl% [name "out"] 
	 [args (new Block% [stmts (list 
				   (new Param% 
					[var-list (list "data")]
					[type "int"] ;; TODO: make it generic
					[known #f]
					[place (new Place% [at "io"])]
					[place-type (new Place% [at "io"])]))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (new Place% [at "io"]))] 
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
		      [place (new Place% [at "io"])])]))
  stdout)

;; returns the ast for a call to digital_write for NODE
(define (get-digital-write node)
  (let ([sym (hash-ref node-to-symbolic-core node)])
    (new FuncDecl% [name (format "digital_write~a" node)]
	 [args (new Block% [stmts (for/list ([i (hash-ref node-to-num-pins
							  node)])
				    (new Param%
					 [var-list (list (format "state~a" i))]
					 [type "int"]
					 [known #f]
					 [place sym]
					 [place-type sym]))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set sym)]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
		      [place #f])])))

;; returns the ast for a call to digital_read for NODE
(define (get-digital-read node)
  (let ([sym (hash-ref node-to-symbolic-core node)])
    (new FuncDecl% [name (format "digital_read~a" node)]
	 [args (new Block% [stmts (list (new Param%
					     [var-list (list "pin")]
					     [type "int"]
					     [known #f]
					     [place sym]
					     [place-type sym]))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set sym)]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "int"]
		      [known #f]
		      [place sym])])))


;; returns the ast for a call to digital_wait for NODE
(define (get-digital-wait node)
  (let ([sym (hash-ref node-to-symbolic-core node)])
    (new FuncDecl% [name (format "digital_wait~a" node)]
	 [args (new Block% [stmts (list (new Param%
                                             [var-list (list "state")]
                                             [type "int"]
                                             [known #f]
                                             [place sym]
                                             [place-type sym]))])]
         [body (new Block% [stmts (list)])]
	 [body-placeset (set sym)]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
                      ;;there is a return value only when in digital nodes
		      [place (if (member node digital-nodes)
                                 sym
                                 #f)])])))

;; returns the ast for a call to delay
(define (get-delay-ns node)
  (let ([sym (hash-ref node-to-symbolic-core node)])
    (new FuncDecl% [name (format "delay_ns~a" node)]
	 [args (new Block% [stmts (list (new Param%
					     [var-list (list "time")]
					     [type "int"]
					     [known #f]
					     [place sym]
					     [place-type sym])
					(new Param%
					     [var-list (list "volts")]
					     [type "int"]
					     [known #f]
					     [place sym]
					     [place-type sym]))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set sym)]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
		      [place #f])])))

(define (get-op exp)
  (get-field op (get-field op exp)))

(define (get-e1 exp)
  (get-field e1 exp))

(define (get-e2 exp)
  (get-field e2 exp))

(define (binop-equal? exp str)
  (and (is-a? exp BinExp%) (equal? (get-op exp) str)))

(define (minus e1 e2 place)
  (if (and (is-a? e2 Num%) (= 0 (get-field n (get-field n e2))))
      e1
      (new BinExp% [op (new Op% [op "-"] [place place])] [e1 e1] [e2 e2])))

(define (same-place? a b)
  ;;(assert (and (place-type? a) (place-type? b)))
  
  (if (and (number? a) (number? b))
      (equal? a b)
      (if (and (place-type-dist? a) (place-type-dist? b))
          (let ([a-list (car a)]
                [b-list (car b)]
                [a-index (cdr a)]
                [b-index (cdr b)])
            (and (and (equal? (length a-list) (length b-list))
                      (equal? (send a-index to-string) (send b-index to-string)))
                 (andmap (lambda (a-p b-p) (send a-p equal-rangeplace? b-p))
                         a-list b-list)))
          ;; if one of them is @any
          (or (at-any? a) (at-any? b) (at-io? a) (at-io? b)))))

(define (unique-place ast)
  (define-syntax-rule (unique a ...)
    (let ([x (get-field place-type ast)])
      (and (number? x) 
	   (equal? x (unique-place (get-field a ast))) ... 
	   x)))

  (cond
   [(is-a? ast Array%) (unique index)]
   [(or (is-a? ast Num%) (is-a? ast Var%)) (unique)]
   [(is-a? ast UnaExp%) (unique e1)]
   [(is-a? ast BinExp%) (unique e1 e2)]
   [else #f]))
   

(define (lookup-name env name)
  (dict-ref env name
            (lambda () (lookup-name (dict-ref env "__up__" 
                                         (lambda () (raise (format "undefined ~a" name))))
                               name))))

(define (lookup env ast)
  (lookup-name env (get-field name ast)))

(define (has-var? env name)
  ;(pretty-display `(has-var? ,env ,name))
  (or (dict-has-key? env name)
      (and (dict-has-key? env "__up__") 
           (has-var? (dict-ref env "__up__") name))))

(define (update-name env name val)
  ;;(pretty-display `(lookup ,env ,name ,val))
  (if (dict-has-key? env name)
      (dict-set! env name val)
      (update-name (dict-ref env "__up__"
                             (lambda () (raise (format "undefine ~a" name))))
                   name val)))

(define (update env ast val)
  (update-name env (get-field name ast) val))

(define (declare env name val)
  ;(pretty-display `(declare ,env ,name ,val))
  (dict-set! env name val))

(define (vector-2d-set! vector n a b val)
  (unless (vector-ref vector a)
     (vector-set! vector a (make-vector n #f)))
  (vector-set! (vector-ref vector a) b val))

(define (vector-2d-ref vector a b)
  (vector-ref (vector-ref vector a) b))

(define (flow x y)
  (define (place-set p)
    (cond
     [(number? p) (set p)]
     [(is-a? p Place%)
      (let ([at (get-field at p)])
        (if (equal? at "any")
            (set)
            (raise "ast-util:place-set doesn't support Place% that is not @any")))]
     [(pair? p)
      (to-place-set p)]

     [(set? p)
      p]

     [(is-a? p TypeExpansion%)
      (set)]

     [else (raise (format "ast-util:place-set unimplemented for ~a" p))]))
    
  (cond
   [(same-place? x y) (cons (set) (set))]
   [else 
    (cons (place-set x) (place-set y))]))

(define (direction me other w h)
  (let ([me-x (floor (/ me w))]
        [me-y (modulo me w)]
        [other-x (floor (/ other w))]
        [other-y (modulo other w)])
    (cond 
     [(= me (* w h))
      other]

     [(= other (* w h))
      `IO]

     [(< other-x me-x)
      (assert (= (add1 other-x) me-x) `(= (add1 other-x) me-x))
      (assert (= other-y me-y) `(= other-y me-y))
      `N]

     [(> other-x me-x)
      (assert (= (sub1 other-x) me-x) `(= (sub1 other-x) me-x))
      (assert (= other-y me-y) `(= other-y me-y))
      `S]

     [(< other-y me-y)
      (assert (= (add1 other-y) me-y) `(= (add1 other-y) me-y))
      (assert (= other-x me-x) `(= other-x me-x))
      `W]

     [(> other-y me-y)
      (assert (= (sub1 other-y) me-y) `(= (sub1 other-y) me-y))
      (assert (= other-x me-x) `(= other-x me-x))
      `E])))


(define (display-list lst)
  (define first (car lst))
  (if (number? first)
      (display first)
      (begin (display "(") (display-list first) (display ")")))
  (for ([i (cdr lst)])
       (display ", ")
       (if (number? i)
           (display i)
           (begin (display "(") (display-list i) (display ")")))))
