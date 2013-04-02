#lang s-exp rosette

(require "partitioner.rkt" rackunit)

(check-equal? 
 (optimize-comm "tests/array-known.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 2)

(check-equal? 
 (optimize-comm "tests/array-dynamic.cll" #:cores 4 #:capacity 256 #:max-msgs 8)
 6)