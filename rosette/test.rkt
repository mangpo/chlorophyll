#lang s-exp rosette

(require "partitioner.rkt" "symbolic-dict.rkt" rackunit)

;; Check with expected number of messages
(define (test-num-msgs name expected-msgs 
                       #:cores [cores 4] [capacity 256] #:max-msgs [max-msgs 8]
                       [file (string-append "tests/" name ".cll")])
  (check-equal? 
   (result-msgs (optimize-comm file #:cores cores #:capacity capacity #:max-msgs max-msgs))
   expected-msgs
   name)
  )

;; Consistency Test
(define (test-consistent name
                         [cores 4] [capacity 256] [max-msgs 8]
                         [file1 (string-append "tests/" name "_concrete.cll")]
                         [file2 (string-append "tests/" name "_symbolic.cll")])
  (let ([res1 (optimize-comm file1 #:cores 4 #:capacity 256 #:max-msgs 8)]
        [res2 (optimize-comm file2 #:cores 4 #:capacity 256 #:max-msgs 8)])
  (check-equal? (result-msgs res1) (result-msgs res2))
  (check-true (cores-equal? (result-cores res1) (result-cores res2)))))

(test-num-msgs "array-known"   2)
(test-num-msgs "array-dynamic" 6)
(test-num-msgs "for-array1"    0)
(test-num-msgs "for-array2"    240 #:max-msgs 300)
(test-num-msgs "for-array3"    0)
(test-num-msgs "for-array3-2"  0)
(test-num-msgs "for-array4"    120 #:max-msgs 300)
(test-num-msgs "for-array5"    720 #:max-msgs 800)
(test-num-msgs "for-array6"    0)
(test-num-msgs "add"           100 #:cores 8 #:max-msgs 200)
(test-num-msgs "function"      2)
(test-num-msgs "function2"     2)
(test-num-msgs "while"         300 #:max-msgs 400)

(test-consistent "space")
(test-consistent "if")