#lang rosette

(provide (all-defined-out))

(define (get-mapping-sym)
  (define-symbolic* map integer?)
  (assert (>= map 0))
  (assert (<= map 1))
  map)

(define (get-sym)
  (define-symbolic* x integer?)
  x)

(define (create-matrix dims init)
  (define (f dims)
    (if (empty? dims)
        (init)
        (let ([vec (make-vector (car dims))])
          (for ([i (car dims)])
            (vector-set! vec i (f (cdr dims))))
          vec)))
  (f dims))

(define-syntax-rule (create-mapping-matrix dims assert-func)
  (create-matrix dims
                 (lambda () (let ([sym (get-sym)])
                              (assert-func sym)
                              sym))))

(define-syntax get
  (syntax-rules ()
    ((get M i) (vector-ref M i))
    ((get M i j ...) (get (vector-ref M i) j ...))))

(define-syntax set
  (syntax-rules ()
    ((get M i v) (vector-set! M i v))
    ((get M i j ...) (set (vector-ref M i) j ...))))