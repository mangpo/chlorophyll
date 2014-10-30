#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '())
    (funcdecl "1if"
      ;; linklist
      (list 

            (block
              "drop "
              1 0 (restrict #t #f #f #f) #f
              "drop ")
            (block
              "65536 "
              0 1 (restrict #t #f #f #f) #f
              "65536 ")
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
0 18 #f)
)
(define name "complexC")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 4 4 #t dir #:id 9))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 4 4 #:id 9)))
