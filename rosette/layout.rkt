#lang s-exp rosette

; TODO: Creating the actual layout map.  See main.rkt.
; A layout map is created for tests in layout-tests/test-layout.rkt

; Note:  Need to check on how equality of symbolic variables works.
; If there are two different symbolic variables a and b representing
; two different physical cores, but then the solver decides to set
; them to the same physical core, what will (= a b) do?  Might it be
; better to use (equal? a b) ?  (This is important for determining the
; estimated size of a core.)

(require "layout-map.rkt")

(provide (all-defined-out))

(define (physical-core x y)
  (cons x y))

(define (physical-core-x core) (car core))
(define (physical-core-y core) (cdr core))
; Need to implement equality so that symbolic variables are compared.
; Hashing is tricky.  Just do a terrible hash function, and make sure
; you never hash cores.
#|(struct physical-core (x y) #:transparent
	#:methods
	gen:equal+hash
	[(define (equal-proc a b equal?-recur)
	   5)
	   (print 
	    (and (= (physical-core-x a) (physical-core-x b))
		 (= (physical-core-y a) (physical-core-y b))))
	   (newline)
	   (and (= (physical-core-x a) (physical-core-x b))
		(= (physical-core-y a) (physical-core-y b))))
	 (define (hash-proc a hash-recur)
	   3)
	 (define (hash2-proc a hash2-recur)
	   5)])|#

(define (make-symbolic-physical-cores n width height)
  (build-list n (lambda (i)
		  (define-symbolic* x number?)
		  (define-symbolic* y number?)
		  (assert (>= x 0))
		  (assert (>= y 0))
		  (assert (< x width))
		  (assert (< y height))
		  (physical-core x y))))

; A procedure to check if two cores are equal that works with symbolic
; cores.  Need to make sure to only compare (symbolic) numbers.
(define (equal-cores? a b)
  (and (= (physical-core-x a) (physical-core-x b))
       (= (physical-core-y a) (physical-core-y b))))

(define (size-of-core layout-map core)
  (foldr + 0 (map val->size (core->vals layout-map core))))
#|
(define (size-of-core layout-map core)
  (define (naive-size-of-core core)
    (apply + (map val->size (core->vals layout-map core))))
  (foldr + 0
	 (map naive-size-of-core
	      (filter (lambda (other) (equal-cores? other core))
		      (all-cores layout-map)))))
|#

(define (manhattan-distance core1 core2)
  (+ (abs (- (physical-core-x core1) (physical-core-x core2)))
     (abs (- (physical-core-y core1) (physical-core-y core2)))))

(define (flatmap fn lst)
  (flatten (map fn lst)))

(define (routing-cost layout-map)
  ; Sum of all of the routing costs
  (apply +
	 ; Loop over each core
	 (flatmap
	  (lambda (core)
	    ; Loop over each value
	    (flatmap
	     (lambda (val)
	       ; Loop over each dependency
	       (map
		(lambda (pair)
		  (let ((other-core (car pair))
			(scale (cdr pair)))
		    ; Estimate routing cost
		    (* scale
		       (manhattan-distance core other-core))))
		(val->dependencies val))) ; List of dependencies
	     (core->vals layout-map core))) ; List of values
	  (all-cores layout-map)))) ; List of all cores

; TODO: Improve max-cost default value
(define (solve-layout layout-map #:capacity [capacity 256]
		      #:max-cost [max-cost 1000])
  (for-each (lambda (core)
              (assert (<= (size-of-core layout-map core) capacity)))
	    (all-cores layout-map))
  (solve (assert (<= (routing-cost layout-map) max-cost)))
  (map (lambda (core)
	 (physical-core (evaluate (physical-core-x core))
			(evaluate (physical-core-y core))))
       (all-cores layout-map)))

; Minimal Testing
(define (run-tests)
  (define test-map (make-layout-map))
  (add-layout-entry! test-map 0 10 (physical-core 1 0)
		     (list (cons (physical-core 2 3) 5)
			   (cons (physical-core 1 1) 2)))
  (print (routing-cost test-map))
  (newline)
  (solve-layout test-map)
  ;(check-layout test-map #:capacity 5)
  (solve #t))

;(run-tests)
