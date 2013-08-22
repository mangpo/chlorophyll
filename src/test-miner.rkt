#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt")

(define program
    ;; core 19
    (aforth 
      ;; list
      (list 
        (vardecl '(8961 43913 56574 21622 0))
        (funcdecl "cadd"
          ;; list
          (list 
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
          )
        )
        (funcdecl "main"
          ;; list
          (list 
            (forloop 
              (block
                "15 "
                0 1 (restrict #t #f #f #f)
                "15 ")
              ;; list
              (list 
                (block
                  "0 b! @b down b! !b 1 b! @b up b! !b "
                  0 0 (restrict #t #f #f #f)
                  "0 b! @b down b! !b 1 b! @b up b! !b ")
                (block
                  "2 b! @b right b! !b 3 b! @b right b! !b "
                  0 0 (restrict #t #f #f #f)
                  "2 b! @b right b! !b 3 b! @b right b! !b ")
                (block
                  "0 b! @b down b! !b "
                  0 0 (restrict #t #f #f #f)
                  "0 b! @b down b! !b ")
                (funccall "cadd")
                (block
                  "0 b! !b 1 b! @b down b! !b dup dup dup"
                  1 0 (restrict #t #f #f #f)
                  "0 b! !b 1 b! @b down b! !b dup dup dup")
                (funccall "cadd")
                (block
                  "1 b! !b 2 b! @b down b! !b dup dup dup"
                  1 0 (restrict #t #f #f #f)
                  "1 b! !b 2 b! @b down b! !b dup dup dup")
                (funccall "cadd")
                (block
                  "2 b! !b 3 b! @b down b! !b dup dup dup"
                  1 0 (restrict #t #f #f #f)
                  "2 b! !b 3 b! @b down b! !b dup dup dup")
                (funccall "cadd")
                (block
                  "3 b! !b dup dup dup"
                  1 0 (restrict #t #f #f #f)
                  "3 b! !b dup dup dup")
              )
              '(#f . #f) 0 16)
          )
        )
      )
    5 18 #hash((0 . 0) (2 . 4) (3 . 5))))

(define-repeating-code program)
