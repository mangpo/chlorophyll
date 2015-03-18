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
          "drop pop "
          1 1 (restrict #t #t #f #f) #f
          "drop pop ")
        (block
          "a "
          0 1 (restrict #t #f #f #f) #f
          "a ")
        (block
          "3 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "3 b! !b ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "3 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "3 b! @b ")
	(block
	 "up b! !b "
	 1 0 (restrict #t #f #f #f) "up"
	 "up b! !b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "2* 2* "
          1 1 (restrict #t #f #f #f) #f
          "2* 2* ")
	(block
	 "up b! @b "
	 0 1 (restrict #t #f #f #f) "up"
	 "up b! @b ")
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
          2 1 (restrict #f #f #f #f) #f
          "+ ")
      )
    #f)
  )
4 18 #f)
)
(define name "complexA")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 4 4 #t dir #:id 1))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 4 4 #:id 11)))
