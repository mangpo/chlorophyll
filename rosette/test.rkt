#lang s-exp rosette

(require "partitioner.rkt" rackunit)

(check-equal? 
 (optimize-comm "tests/array-known.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 2)

(check-equal? 
 (optimize-comm "tests/array-dynamic.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 6)

(check-equal? 
 (optimize-comm "tests/for-array1.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 0)

(check-equal? 
 (optimize-comm "tests/for-array2.cll" #:cores 4 #:capacity 256 #:max-msgs 300)
 240)

(check-equal? 
 (optimize-comm "tests/for-array3.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 0)

(check-equal? 
 (optimize-comm "tests/for-array4.cll" #:cores 4 #:capacity 256 #:max-msgs 200)
 120)

(check-equal? 
 (optimize-comm "tests/for-array5.cll" #:cores 4 #:capacity 256 #:max-msgs 800)
 720)


(check-equal? 
 (optimize-comm "tests/for-array6.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 0)