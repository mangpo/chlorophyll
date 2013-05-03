#lang s-exp rosette

(require "partitioner.rkt" "symbolic-dict.rkt" rackunit)

;; Check with expected number of messages
(define (test-num-msgs file expected-msgs #:cores [cores 4] [capacity 256] #:max-msgs [max-msgs 8])
  (check-equal? 
   (result-msgs (optimize-comm file #:cores cores #:capacity capacity #:max-msgs max-msgs))
   expected-msgs))

;; Consistency Test
(define (test-consistent file1 file2 [cores 4] [capacity 256] [max-msgs 8])
  (let ([res1 (optimize-comm file1 #:cores 4 #:capacity 256 #:max-msgs 8)]
        [res2 (optimize-comm file2 #:cores 4 #:capacity 256 #:max-msgs 8)])
  (check-equal? (result-msgs res1) (result-msgs res2))
  (check-true (cores-equal? (result-cores res1) (result-cores res2)))))

(test-num-msgs "tests/array-known.cll"   2)
(test-num-msgs "tests/array-dynamic.cll" 6)
(test-num-msgs "tests/for-array1.cll"    0)
(test-num-msgs "tests/for-array2.cll"    240 #:max-msgs 300)
(test-num-msgs "tests/for-array3.cll"    0)
(test-num-msgs "tests/for-array4.cll"    120 #:max-msgs 300)
(test-num-msgs "tests/for-array5.cll"    720 #:max-msgs 800)
(test-num-msgs "tests/for-array6.cll"    0)
(test-num-msgs "tests/add.cll"           100 #:cores 8 #:max-msgs 200)

(test-consistent "tests/space_concrete.cll" "tests/space_symbolic.cll")
(test-consistent "tests/if_concrete.cll" "tests/if_symbolic.cll")