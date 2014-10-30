#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0))
    (funcdecl "main"
      ;; linklist
      (list 
       
        (block
          "drop "
          1 0 (restrict #t #f #f #f) #f
          "drop ")
        (block
          "3 "
          0 1 (restrict #t #f #f #f) #f
          "3 ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "+ "
          2 1 (restrict #t #f #f #f) #f
          "+ ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
	(block
	 "dup "
	 1 2 (restrict #t #f #f #f) #f
	 "dup ")
        (block
          "0 b! @b "
          0 1 (restrict #f #f #f #f) #f
          "0 b! @b ")
      )
    #f)
  )
1 18 #f)
)
(define name "complexB")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 4 4 #t dir #:id 11))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 4 4 #:id 11)))
