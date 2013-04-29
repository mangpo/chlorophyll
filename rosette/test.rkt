#lang s-exp rosette

(require "partitioner.rkt" "symbolic-dict.rkt" rackunit)

(check-equal? 
 (result-msgs (optimize-comm "tests/array-known.cll" #:cores 4 #:capacity 256 #:max-msgs 8))
 2)

(check-equal? 
 (result-msgs (optimize-comm "tests/array-dynamic.cll" #:cores 4 #:capacity 256 #:max-msgs 8))
 6)

(check-equal? 
 (result-msgs (optimize-comm "tests/for-array1.cll" #:cores 4 #:capacity 256 #:max-msgs 8))
 0)

(check-equal? 
 (result-msgs (optimize-comm "tests/for-array2.cll" #:cores 4 #:capacity 256 #:max-msgs 300))
 240)

(check-equal? 
 (result-msgs (optimize-comm "tests/for-array3.cll" #:cores 4 #:capacity 256 #:max-msgs 8))
 0)

(check-equal? 
 (result-msgs (optimize-comm "tests/for-array4.cll" #:cores 4 #:capacity 256 #:max-msgs 200))
 120)

(check-equal? 
 (result-msgs (optimize-comm "tests/for-array5.cll" #:cores 4 #:capacity 256 #:max-msgs 800))
 720)


(check-equal? 
 (result-msgs (optimize-comm "tests/for-array6.cll" #:cores 4 #:capacity 256 #:max-msgs 8))
 0)

;; Consistency Test
(let ([res1 (optimize-comm "tests/space_concrete.cll" #:cores 4 #:capacity 256 #:max-msgs 8)]
      [res2 (optimize-comm "tests/space_symbolic.cll" #:cores 4 #:capacity 256 #:max-msgs 8)])
  (check-equal? (result-msgs res1) (result-msgs res2))
  (check-true (cores-equal? (result-cores res1) (result-cores res2))))