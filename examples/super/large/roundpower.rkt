#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '())
    (funcdecl "main"
      ;; linklist
      (list 
        (block
          "0 "
          0 1 (restrict #t #f #f #f) #f
          "0 ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "1 "
          0 1 (restrict #t #f #f #f) #f
          "1 ")
        (block
          "- 1 + + "
          2 1 (restrict #t #f #f #f) #f
          "- 1 + + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "2/ 2/ 2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ 2/ 2/ ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "1 "
          0 1 (restrict #t #f #f #f) #f
          "1 ")
        (block
          "+ "
          2 1 (restrict #t #f #f #f) #f
          "+ ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
      )
    #f)
  )
0 18 #f)
)
(define name "roundpower")
(define dir "super/large")
(define time (current-seconds))
(define real-opts (superoptimize program name 2 3 #t dir #:id 5))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 2 3 #:id 5)))
