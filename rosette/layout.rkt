#lang s-exp rosette

; TODO: Creating the actual layout map.  See main.rkt.

; Note:  Need to check on how equality of symbolic variables works.
; If there are two different symbolic variables a and b representing
; two different physical cores, but then the solver decides to set
; them to the same physical core, what will (= a b) do?  Might it be
; better to use (equal? a b) ?  (This is important for determining the
; estimated size of a core.)

(require "layout-map.rkt")

(struct physical-core (x y))

(define (size-of-core layout-map core)
  (apply + (map val->size (core->vals layout-map core))))

(define (check-layout layout-map #:capacity [capacity 256])
  (for-each (lambda (core)
              (assert (<= (size-of-core layout-map core) capacity)))
	    (all-cores layout-map)))

(define (manhattan-distance core1 core2)
  (+ (abs (- (physical-core-x core1) (physical-core-x core2)))
     (abs (- (physical-core-y core1) (physical-core-y core2)))))

(define (flatmap fn lst)
  (foldr append '() (map fn lst)))

(define (min-routing-cost layout-map)
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

; Minimal Testing

(define test-map (make-layout-map))
(add-layout-entry! test-map 0 10 (physical-core 1 0)
		   (list (cons (physical-core 2 3) 5)
			 (cons (physical-core 1 1) 2)))
(print (min-routing-cost test-map))
(newline)
(check-layout test-map)
;(check-layout test-map #:capacity 5)
(solve #t)
