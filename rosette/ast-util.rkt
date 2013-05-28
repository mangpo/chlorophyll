#lang s-exp rosette

(require "header.rkt" "ast.rkt")

(provide (all-defined-out))

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
          (or (and (is-a? a Place%) (equal? (get-field at a) "any"))
              (and (is-a? b Place%) (equal? (get-field at b) "any")))))
  )

(define (lookup-name env name)
  (dict-ref env name
            (lambda () (lookup (dict-ref env "__up__" 
                                         (lambda () (raise (format "undefined ~a" name))))
                               name))))

(define (lookup env ast)
  (dict-ref env (get-field name ast)
            (lambda () (lookup (dict-ref env "__up__" 
                                         (lambda () (send ast not-found-error)))
                               ast))))

(define (update env ast val)
  (let ([name (get-field name ast)])
    (if (dict-has-key? env name)
        (dict-set! env name val)
            (update (dict-ref env "__up__"
                              (lambda () (send ast not-found-error))) ast val))))

(define (declare env name val)
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

     [else (raise (format "ast-util:place-set unimplemented for ~a" p))]))
    
  (cond
   [(same-place? x y) (cons (set) (set))]
   [else 
    (cons (place-set x) (place-set y))]))

(define (direction me other w)
  (let ([me-x (floor (/ me w))]
        [me-y (modulo me w)]
        [other-x (modulo other w)]
        [other-y (floor (/ other w))])
    (cond 
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


      
        
                  