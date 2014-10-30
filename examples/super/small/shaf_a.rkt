#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0))
    (funcdecl "f1"
      ;; linklist
      (list 
        (block
          "0 a! !+ !+ push pop "
          3 1 (restrict #t #f #f #f) #f
          "0 a! !+ !+ push pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "over "
          0 1 (restrict #t #f #f #f) #f
          "over ")
        (block
          "- "
          1 1 (restrict #t #f #f #f) #f
          "- ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "or "
          2 1 (restrict #t #f #f #f) #f
          "or ")
        (block
          "push drop pop "
          2 1 (restrict #f #f #f #f) #f
          "push drop pop ")
      )
    '(#f #f #f)))
  2 18 #f)
)
(define name "sha_fa")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 2 3 #t dir #:id 1))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 2 3 #:id 1)))
