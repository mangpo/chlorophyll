#lang racket
(require "../../../src/header.rkt" "../../../src/arrayforth.rkt" "../../../src/arrayforth-optimize.rkt" "../../../src/arrayforth-print.rkt")
(define program
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0))
    (funcdecl "f"
      ;; linklist
      (list 
        (block
          "@b"
          0 1 (restrict #t #f #f #f) #f
          "@b")
        (block
          "87381 "
          0 1 (restrict #t #f #f #f) #f
          "87381 ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "- 1 + + "
          2 1 (restrict #t #f #f #f) #f
          "- 1 + + ")
        (block
          "push drop pop "
          2 1 (restrict #t #f #f #f) #f
          "push drop pop ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "209715 "
          0 1 (restrict #t #f #f #f) #f
          "209715 ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (block
          "2/ 2/ "
          1 1 (restrict #t #f #f #f) #f
          "2/ 2/ ")
        (block
          "1 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "1 b! !b ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
        (block
          "209715 "
          0 1 (restrict #t #f #f #f) #f
          "209715 ")
        (block
          "and "
          2 1 (restrict #t #f #f #f) #f
          "and ")
        (block
          "1 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "1 b! !b ")
        (block
          "0 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "0 b! @b ")
        (block
          "1 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "1 b! @b ")
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
          0 1 (restrict #t #f #f #f) #f
          "dup ")
      )
    #f)
  )
2 18 #f)
)
(define name "count_a")
(define dir "super/large")
(define time (current-seconds))
(define real-opts (superoptimize program name 2 3 #t dir #:id 1))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.time" dir name) (lambda () (pretty-display (- (current-seconds) time))))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.rkt" dir name) (lambda () (aforth-struct-print real-opts)))
(with-output-to-file #:exists 'truncate (format "~a/~a-opt.aforth" dir name)
(lambda () (aforth-syntax-print real-opts 2 3 #:id 5)))
