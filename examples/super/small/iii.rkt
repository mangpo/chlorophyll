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
          "0 a! !+ push !+ pop "
          3 1 (restrict #t #f #f #f) #f
          "0 a! !+ push !+ pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "65535 "
          0 1 (restrict #t #f #f #f) #f
          "65535 ")
        (block
          "or "
          2 1 (restrict #t #f #f #f) #f
          "or ")
        (block
          "over - and + "
          2 1 (restrict #t #f #f #f) #f
          "over - and + ")
        (block
          "or "
          2 1 (restrict #t #f #f #f) #f
          "or ")
        (block
          "push drop pop "
          2 1 (restrict #f #f #f #f) #f
          "push drop pop ")
      )
    '((<= . 65535) (<= . 65535) (<= . 65535)))
  )
3 18 #f)
)
(define name "iii")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 8 8 #t dir #:id 36))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 8 8 #:id 36)))
