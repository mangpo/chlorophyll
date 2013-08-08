#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" "arrayforth-print.rkt")

(define program
    ;; core 18
    (aforth 
      ;; list
      (list 
        (vardecl '(5 3 2 3 5 5 1 2 2 2 3 6 0 0 5 0 2 3 1 4 2 3 6 0 4 5 3 3 4 4 1 4 5 5 5 3 0 0 0 0 0 0 0 0 0 0))
        (funcdecl "main"
          ;; list
          (list 
            (forloop 
              (block
                "5 "
                0 1 (restrict #t #f #f)
                "5 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "37 a! 5 "
                    0 1 (restrict #t #t #f)
                    "37 a! 5 ")
                  ;; list
                  (list 
                    (block
                      "right b! @b !+ "
                      0 0 (restrict #t #t #f)
                      "right b! @b !+ ")
                  )
                  '((37 . opt) 37 . opt) 0 6)
                (forloop 
                  (block
                    "5 "
                    0 1 (restrict #t #f #f)
                    "5 ")
                  ;; list
                  (list 
                    (block
                      "0 36 b! !b "
                      0 0 (restrict #t #f #f)
                      "0 36 b! !b ")
                    (forloop 
                      (block
                        "37 a! 5 "
                        0 1 (restrict #t #t #f)
                        "37 a! 5 ")
                      ;; list
                      (list 
                        (block
                          "36 b! @b @+ 6 45 b! @b "
                          0 3 (restrict #t #t #f)
                          "36 b! @b @+ 6 45 b! @b ")
                        (mult)
                        (block
                          "44 b! @b + b! @b "
                          1 1 (restrict #t #t #f)
                          "44 b! @b + b! @b ")
                        (mult)
                        (block
                          "+ 36 b! !b "
                          2 0 (restrict #t #t #f)
                          "+ 36 b! !b ")
                      )
                      '((37 . opt) 37 . opt) 0 6)
                    (block
                      "36 b! @b left b! !b "
                      0 0 (restrict #t #f #f)
                      "36 b! @b left b! !b ")
                  )
                  '(#f . #f) 0 6)
                (block
                  ""
                  0 0 (restrict #t #f #f)
                  "")
              )
              '(#f . #f) 0 6)
          )
        )
      )
    46 18 #hash((0 . 0) (2 . 36) (3 . 37) (5 . 43) (6 . 44) (7 . 45) (8 . 46))))

(aforth-syntax-print (define-repeating-code program) 1 1)