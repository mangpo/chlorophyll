#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0 0))
    (funcdecl "p15_ceil_avg"
      ;; linklist
      (list 
        (block
          "0 a! !+ !+ "
          2 0 (restrict #t #f #f #f) #f
          "0 a! !+ !+ ")
        (block
          "0 "
          0 1 (restrict #t #f #f #f) #f
          "0 ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "2 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "2 b! !b ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "or "
          2 1 (restrict #t #f #f #f) #f
          "or ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "2 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "2 b! @b ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "- 1 + + "
          2 1 (restrict #t #f #f #f) #f
          "- 1 + + ")
        (block
          "push drop pop "
          2 1 (restrict #f #f #f #f) #f
          "push drop pop ")
      )
    #f)
  )
3 18 #f)
)
(define name "ceil")
(define dir "super/large")
(define time (current-seconds))
(define real-opts (superoptimize program name 2 3 #t dir #:id 5))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 2 3 #:id 5)))
