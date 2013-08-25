#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt")

(define program
    ;; core 21
    (aforth 
      ;; list
      (list 
        (vardecl '(0 0 0 0))
        (funcdecl "sumrotate"
          ;; list
          (list 
            (block
              "0 a! !+ "
              1 0 (restrict #t #f #f #f)
              "0 a! !+ ")
            (block
              "0 b! @b up b! !b down b! @b up b! !b up b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b up b! !b down b! @b up b! !b up b! @b ")
          )
        )
        (funcdecl "main"
          ;; list
          (list 
            (block
              "up b! @b 1 b! !b up b! @b 2 b! !b "
              0 0 (restrict #t #f #f #f)
              "up b! @b 1 b! !b up b! @b 2 b! !b ")
            (forloop 
              (block
                "15 "
                0 1 (restrict #t #f #f #f)
                "15 ")
              ;; list
              (list 
                (block
                  "1 b! @b right b! !b 2 b! @b right b! !b "
                  0 0 (restrict #t #f #f #f)
                  "1 b! @b right b! !b 2 b! @b right b! !b ")
                (block
                  "1 b! @b right b! !b 1 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "1 b! @b right b! !b 1 b! @b ")
                (funccall "sumrotate")
                (block
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b ")
                (block
                  "3 b! @b 1 b! !b "
                  0 0 (restrict #t #f #f #f)
                  "3 b! @b 1 b! !b ")
              )
              '(#f . #f) 0 16)
            (forloop 
              (block
                "31 "
                0 1 (restrict #t #t #f #t)
                "31 ")
              ;; list
              (list 
                (block
                  "1 b! @b right b! !b 2 b! @b right b! !b 1 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "1 b! @b right b! !b 2 b! @b right b! !b 1 b! @b ")
                (funccall "sumrotate")
                (block
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b ")
                (block
                  "3 b! @b 1 b! !b "
                  0 0 (restrict #t #f #f #f)
                  "3 b! @b 1 b! !b ")
              )
              '(#f . #f) 16 48)
            (forloop 
              (block
                "15 "
                0 1 (restrict #t #f #f #f)
                "15 ")
              ;; list
              (list 
                (block
                  "2 b! @b right b! !b 1 b! @b right b! !b 1 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "2 b! @b right b! !b 1 b! @b right b! !b 1 b! @b ")
                (funccall "sumrotate")
                (block
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "3 b! !b 2 b! @b up b! !b 1 b! @b 2 b! !b ")
                (block
                  "3 b! @b 1 b! !b "
                  0 0 (restrict #t #f #f #f)
                  "3 b! @b 1 b! !b ")
              )
              '(#f . #f) 48 64)
            (block
              "1 b! @b up b! !b 2 b! @b up b! !b "
              0 0 (restrict #t #f #f #f)
              "1 b! @b up b! !b 2 b! @b up b! !b ")
          )
        )
      )
    4 18 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4))))

(define-repeating-code program)
