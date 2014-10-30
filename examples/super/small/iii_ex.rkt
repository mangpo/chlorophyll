#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0 0))
    (funcdecl "iii"
      ;; linklist
      (list 
        (block
          "a! "
          1 0 (restrict #t #t #f #f) #f
          "a! ")
        (block
          "push "
          1 0 (restrict #t #t #f #t) #f
          "push ")
        (block
          "a "
          0 1 (restrict #t #f #f #t) #f
          "a ")
        (block
          "- "
          1 1 (restrict #t #f #f #t) #f
          "- ")
        (block
          "over "
          2 3 (restrict #t #f #f #t) #f
          "over ")
        (block
          "65535 "
          0 1 (restrict #t #f #f #t) #f
          "65535 ")
        (block
          "or "
          2 1 (restrict #t #f #f #t) #f
          "or ")
        (block
          "and "
          2 1 (restrict #t #f #f #t) #f
          "and ")
        (block
          "or "
          2 1 (restrict #t #f #f #t) #f
          "or ")
        (block
          "pop "
          0 1 (restrict #t #f #f #f) #f
          "pop ")
        (block
          "or "
          2 1 (restrict #f #f #f #f) #f
          "or ")
      )
    '((<= . 65535) (<= . 65535) (<= . 65535)))
  )
3 18 #f)
)
(define name "iii_ex")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 8 8 #t dir #:id 36))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 8 8 #:id 36)))
