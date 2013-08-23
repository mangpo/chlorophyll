#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt")

(define program
    ;; core 4
    (aforth 
      ;; list
      (list 
        (vardecl '(0 0 0))
        (funcdecl "cadd"
          ;; list
          (list 
            (block
              "0 a! !+ !+ "
              2 0 (restrict #t #f #f #f)
              "0 a! !+ !+ ")
            (block
              "1 b! @b 0 b! @b + right b! @b 3 and + 2 b! !b "
              0 0 (restrict #t #f #f #f)
              "1 b! @b 0 b! @b + right b! @b 3 and + 2 b! !b ")
            (block
              "2 b! @b 65535 and 2 b! !b 2 b! @b "
              0 1 (restrict #t #f #f #f)
              "2 b! @b 65535 and 2 b! !b 2 b! @b ")
          )
        )
        (funcdecl "sumrotate"
          ;; list
          (list 
            (block
              "left b! @b right b! !b "
              0 0 (restrict #t #f #f #f)
              "left b! @b right b! !b ")
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
                  "down b! @b right b! !b "
                  0 0 (restrict #t #f #f #f)
                  "down b! @b right b! !b ")
                (forloop 
                  (block
                    "63 "
                    0 1 (restrict #t #t #f #t)
                    "63 ")
                  ;; list
                  (list 
                    (funccall "sumrotate")
                    (block
                      ""
                      0 0 (restrict #f #f #f #f)
                      "")
                  )
                  '(#f . #f) 0 64)
                (block
                  "down b! @b right b! @b "
                  0 2 (restrict #t #f #f #f)
                  "down b! @b right b! @b ")
                (funccall "cadd")
                (block
                  "down b! !b down b! @b right b! @b "
                  1 2 (restrict #t #f #f #f)
                  "down b! !b down b! @b right b! @b ")
                (funccall "cadd")
                (block
                  "down b! !b down b! @b right b! @b "
                  1 2 (restrict #t #f #f #f)
                  "down b! !b down b! @b right b! @b ")
                (funccall "cadd")
                (block
                  "down b! !b down b! @b right b! @b "
                  1 2 (restrict #t #f #f #f)
                  "down b! !b down b! @b right b! @b ")
                (funccall "cadd")
                (block
                  "down b! !b "
                  1 0 (restrict #t #f #f #f)
                  "down b! !b ")
              )
              '(#f . #f) 0 16)
          )
        )
      )
    3 18 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3))))

(define-repeating-code program)
