#lang s-exp rosette

(require "parser.rkt" 
         "compiler.rkt"
         "partitioner.rkt" 
	 "partition-storage.rkt" 
	 rackunit)

(define testdir "../tests/")

(define (optimize-file file cores capacity max-msgs)
  (define my-ast (parse file))
  (optimize-comm my-ast #:cores cores #:capacity capacity #:max-msgs max-msgs 
                 #:verbose #t))

;(optimize-file "../examples/test.cll" 4 1024 #f)
(optimize-file "../tests/run/md5-noplace.cll" 64 720 #f)

;; Check with expected number of messages
(define (test-num-msgs name expected-msgs 
                       #:cores [cores 4] #:capacity [capacity 256] #:max-msgs [max-msgs 8]
                       [file (string-append testdir name ".cll")])
  (check-equal? 
   (result-msgs (optimize-file file cores capacity max-msgs))
   expected-msgs
   name)
  )

;; Consistency Test
(define (test-consistent name
                         [cores 4] [capacity 256] [max-msgs 8]
                         [file1 (string-append testdir name "_concrete.cll")]
                         [file2 (string-append testdir name "_symbolic.cll")])
  (let ([res1 (optimize-file file1 cores capacity max-msgs)]
        [res2 (optimize-file file2 cores capacity max-msgs)])
  (check-equal? (result-msgs res1) (result-msgs res2))
    (check-true (cores-equal? (result-cores res1) (result-cores res2)))))

;; No error test
(define (no-error name [cores 4] [capacity 256] [max-msgs #f]
                  [file (string-append testdir name ".cll")])
  (optimize-file file cores capacity max-msgs))

;(test-num-msgs "for-array1"    0)
;(test-num-msgs "for-array3"    0)
;(test-num-msgs "for-array3-2"  0)
;(test-num-msgs "for-array4"     20 #:max-msgs 100 #:capacity 512)
;(test-num-msgs "for-array6"    0)
;(test-num-msgs "add"           100 #:cores 8 #:max-msgs 200 #:capacity 300)
;(test-num-msgs "add-pair"      100 #:cores 8 #:max-msgs 200 #:capacity 300)
;(test-num-msgs "function"      2 #:capacity 512)
;(test-num-msgs "function2"     4)
;(test-num-msgs "while"         300 #:max-msgs 800)

;(test-consistent "space")
;(test-consistent "if")
