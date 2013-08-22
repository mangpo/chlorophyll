#lang s-exp rosette

(require "header.rkt" "ast.rkt")

(provide (all-defined-out))

(define (ext-name name ext)
  (format "~a::~a" name ext))

(define io-uid 0)

(define (get-input-func-pull src)
  (set! io-uid (add1 io-uid))
  (define output-vardecl (get-field output-vardecl src))
  (define stdin
    (new GlobalIOFuncDecl% [name (format "in#from#~a#~a" (get-field name src) io-uid)] 
	 [args (new Block% [stmts (list)])] 
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (get-field place output-vardecl))]
	 [return (new VarDecl% [var-list (list "#return")]
                      [type (get-field type output-vardecl)]
                      [place (get-field place output-vardecl)]
                      [known #f]
                      )]))
  stdin)

(define (get-input-func-made-available this source)
  (set! io-uid (add1 io-uid))
  (define input-vardecl (get-field input-vardecl this))
  (define stdin
    (new FilterInputFuncDecl%
         [name (format "in#from#~a#inside#~a#~a"
                       (get-field name source)
                       (get-field name this)
                       io-uid)] 
	 [args (new Block% [stmts (list)])] 
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (get-field place input-vardecl))]
         [this-filter this]
         [source-filter source]
	 [return input-vardecl]))
  stdin)

(define (get-output-func-push dst)
  (set! io-uid (add1 io-uid))
  (define input-vardecl (get-field input-vardecl dst))
  (define stdout
    (new GlobalIOFuncDecl% [name (format "out#to#~a#~a" (get-field name dst) io-uid)] 
	 [args (new Block% [stmts (list (new Param%
                                             [var-list (list "data")]
                                             [type (get-field type input-vardecl)]
                                             [known #f]
                                             [place (get-field place input-vardecl)]
                                             [place-type (get-field place input-vardecl)]
                                             ))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (get-field place input-vardecl))]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
		      [place (new Place% [at "output"])]
                      )]))
  stdout)

(define (get-output-func-make-available this destination)
  (set! io-uid (add1 io-uid))
  (define output-vardecl (get-field output-vardecl this))
  (define stdout
    (new FilterOutputFuncDecl%
         [name (format "out#to#~a#inside#~a#~a"
                       (get-field name destination)
                       (get-field name this)
                       io-uid)] 
	 [args (new Block% [stmts (list (new Param%
                                             [var-list (list "data")]
                                             [type (get-field type output-vardecl)]
                                             [known #f]
                                             [place (get-field place output-vardecl)]
                                             [place-type (get-field place output-vardecl)]
                                             ))])]
	 [body (new Block% [stmts (list)])]
	 [body-placeset (set (get-field place output-vardecl))]
         [this-filter this]
         [destination-filter destination]
	 [return (new VarDecl% [var-list (list "#return")]
		      [type "void"]
		      [known #f]
		      [place (new Place% [at "output"])]
                      )]))
  stdout)

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
          (or (at-any? a) (at-any? b)
              (at-global-input? a) (at-global-output? a)
              (at-global-input? b) (at-global-output? b)
              ))))

(define (lookup-name env name)
  (dict-ref env name
            (lambda () (lookup-name (dict-ref env "__up__" 
                                         (lambda () (raise (format "undefined ~a" name))))
                               name))))

(define (lookup env ast)
  ;(pretty-display `(lookup ,env ,(send ast to-string)))
  (dict-ref env (get-field name ast)
            (lambda () (lookup (dict-ref env "__up__" 
                                         (lambda () (send ast not-found-error)))
                               ast))))

(define (update env ast val)
  (let ([name (get-field name ast)])
    ;(pretty-display `(lookup ,env ,name ,val))
    (if (dict-has-key? env name)
        (dict-set! env name val)
            (update (dict-ref env "__up__"
                              (lambda () (send ast not-found-error))) ast val))))

(define (update-name env name val)
  ;(pretty-display `(lookup ,env ,name ,val))
  (if (dict-has-key? env name)
      (dict-set! env name val)
      (update-name (dict-ref env "__up__"
                             (lambda () (raise (format "undefined ~a" name)))) name val)))

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
      (cons `INPUT other)]

     [(= other (* w h))
      `INPUT]

     [(= me (add1 (* w h)))
      (cons `OUTPUT other)]

     [(= other (add1 (* w h)))
      `OUTPUT]

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

