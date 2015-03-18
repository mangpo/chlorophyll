#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0 0 0 0))

    (funcdecl "leftrotate"
      ;; linklist
      (list 
        (block
          "0 b! !b "
          0 1 (restrict #f #f #f #f) #f
          "0 b! !b ")
      )
    #f)
  )
5 18 #f)
)
(define name "test")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 8 8 #t dir #:id 26))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 8 8 #:id 26)))
