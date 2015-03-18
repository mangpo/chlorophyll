#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(8192 8192 16384 32768 32768 65536 65536 131071 131071 131071 131071 65536 65536 32768 32768 16384 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
    (funcdecl "main"
      ;; linklist
	 (list
            (block
              "push drop pop "
              2 1 (restrict #t #t #f #f) #f
              "push drop pop ")
            (block
              "pop a! "
              0 0 (restrict #t #t #f #t) #f
              "pop a! ")
            (block
              "right b! !b "
              1 0 (restrict #t #t #f #f) "right"
              "right b! !b ")
            (block
              "dup "
              0 1 (restrict #t #t #f #f) #f
              "dup ")
            (block
              "1 "
              0 1 (restrict #t #t #f #f) #f
              "1 ")
            (block
              "+ "
              2 1 (restrict #t #t #f #f) #f
              "+ ")
            (block
              "15 "
              0 1 (restrict #t #t #f #f) #f
              "15 ")
            (block
              "and "
              2 1 (restrict #t #t #f #f) #f
              "and ")
            (block
              "push drop pop "
              2 1 (restrict #t #t #f #f) #f
              "push drop pop ")
          )
    #f)
  )
4 18 #hash((0 . 0) (2 . 16) (4 . 32)))
)
(define name "fir")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 3 3 #t dir #:id 4))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 3 3 #:id 4)))
