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
          "2 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "2 b! !b ")

	(block
	 "push drop pop "
	 2 1 (restrict #t #f #f #f) #f
	 "push drop pop ")

        (block
          "2 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "2 b! @b ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")

        (block
          "up b! @b "
          0 1 (restrict #t #f #f #f) "up"
          "up b! @b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ ")
        (block
          "+ "
          2 1 (restrict #t #f #f #f) #f
          "+ ")
        (block
          "65535 "
          0 1 (restrict #t #f #f #f) #f
          "65535 ")
        (block
          "and "
          2 1 (restrict #f #f #f #f) #f
          "and ")
      )
    #f)
  )
5 18 #f)
)
(define name "rrotate")
(define dir "super/small")
(define time (current-seconds))
(define real-opts (superoptimize program name 8 8 #t dir #:id 26))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 8 8 #:id 26)))
