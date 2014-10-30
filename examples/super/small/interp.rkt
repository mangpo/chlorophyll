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
          2 1 (restrict #f #f #f #f) #f
          "- 1 + + ")
      )
    #f)
  )
4 18 #hash((0 . 0) (2 . 9) (3 . 10) (4 . 11)))
)
(define name "interp")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 3 3 #t dir #:id 7))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 3 3 #:id 7)))
