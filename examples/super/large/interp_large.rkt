#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 450 900 1350 1800 2250 2700 3150 3600 0 0))
    (funcdecl "main"
      ;; linklist
      (list 
        (block
          "0 "
          0 1 (restrict #t #f #f #f) #f
          "0 ")
        (block
          "right b! @b "
          0 1 (restrict #t #f #f #f) "right"
          "right b! @b ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "63 "
          0 1 (restrict #t #f #f #f) #f
          "63 ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "3 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "10 b! !b ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "2/ 2/ 2/ 2/ 2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ 2/ 2/ 2/ 2/ ")
        (block
          "2 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "9 b! !b ")
        (block
          "2 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "9 b! @b ")
        (block
          "b! @b "
          1 1 (restrict #t #f #f #f) #f
          "b! @b ")
        (block
          "2 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "9 b! @b ")
        (block
          "1 + b! @b "
          1 1 (restrict #t #f #f #f) #f
          "1 + b! @b ")
        (block
          "2 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "9 b! @b ")
        (block
          "b! @b "
          1 1 (restrict #t #f #f #f) #f
          "b! @b ")
        (block
          "- 1 + + "
          2 1 (restrict #t #f #f #f) #f
          "- 1 + + ")
        (block
          "3 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "10 b! @b ")
      )
    #f)
  )
4 18 #hash((0 . 0) (2 . 9) (3 . 10) (4 . 11)))
)
(define name "interp-large")
(define dir "super/large")
(define time (current-seconds))
(define real-opts (superoptimize program name 3 3 #t dir #:id 7))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 3 3 #:id 7)))
