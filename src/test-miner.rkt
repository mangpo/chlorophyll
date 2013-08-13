#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" "arrayforth-print.rkt")

(define program
    ;; core 18
    (aforth 
      ;; list
      (list 
        (vardecl '(0))
        (funcdecl "1while"
          ;; list
          (list 
            (block
              "0 b! @b 10 - 1 + + dup left b! !b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b 10 - 1 + + dup left b! !b ")
            (-ift 
              ;; list
              (list 
                (block
                  "10 0 b! @b + left b! !b 0 b! @b left b! !b "
                  0 0 (restrict #t #f #f #f)
                  "10 0 b! @b + left b! !b 0 b! @b left b! !b ")
                (block
                  "0 b! @b 2 + 0 b! !b "
                  0 0 (restrict #t #f #f #f)
                  "0 b! @b 2 + 0 b! !b ")
                (funccall "1while")
              )
            )
          )
        )
        (funcdecl "main"
          ;; list
          (list 
            (block
              "0 0 b! !b "
              0 0 (restrict #t #f #f #f)
              "0 0 b! !b ")
            (funccall "1while")
          )
        )
      )
    1 18 #hash((0 . 0) (1 . 1))))

(aforth-syntax-print (define-repeating-code program) 1 1)