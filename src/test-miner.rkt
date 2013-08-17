#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt")

(define program
  ;; core 16
    (aforth 
      ;; list
      (list 
        (vardecl '(0 0 0 0))
        (funcdecl "leftrotate"
          ;; list
          (list 
            (block
              "up b! @b 1 b! !b up b! @b 2 b! !b up b! @b "
              0 1 (restrict #t #f #f #f)
              "up b! @b 1 b! !b up b! @b 2 b! !b up b! @b ")
            (block
              "0 b! !b 2 b! @b 16 0 b! @b - 1 + + "
              1 2 (restrict #t #f #f #f)
              "0 b! !b 2 b! @b 16 0 b! @b - 1 + + ")
            (ift 
              ;; list
              (list 
                (block
                  "-1 + "
                  1 1 (restrict #t #f #f #f)
                  "-1 + ")
                (forloop 
                  (block
                    ""
                    0 0 (restrict #t #f #f #f)
                    "")
                  ;; list
                  (list 
                    (block
                      "2/ "
                      1 1 (restrict #t #f #f #f)
                      "2/ ")
                  )
                  #f #f #f)
                (block
                  "dup "
                  1 2 (restrict #t #f #f #f)
                  "dup ")
              )
            )
            (block
              "drop up b! !b 1 b! @b 16 0 b! @b - 1 + + "
              2 2 (restrict #t #f #f #f)
              "drop up b! !b 1 b! @b 16 0 b! @b - 1 + + ")
            (ift 
              ;; list
              (list 
                (block
                  "-1 + "
                  1 1 (restrict #t #f #f #f)
                  "-1 + ")
                (forloop 
                  (block
                    ""
                    0 0 (restrict #t #f #f #f)
                    "")
                  ;; list
                  (list 
                    (block
                      "2/ "
                      1 1 (restrict #t #f #f #f)
                      "2/ ")
                  )
                  #f #f #f)
                (block
                  "dup "
                  1 2 (restrict #t #f #f #f)
                  "dup ")
              )
            )
            (block
              "drop up b! !b "
              2 0 (restrict #t #f #f #f)
              "drop up b! !b ")
          )
        )
        (funcdecl "sumrotate"
          ;; list
          (list 
            (funccall "leftrotate")
          )
        )
        (funcdecl "main"
          ;; list
          (list 
            (forloop 
              (block
                "0 "
                0 1 (restrict #t #f #f #f)
                "0 ")
              ;; list
              (list 
                (funccall "sumrotate")
                (block
                  ""
                  0 0 (restrict #t #f #f #f)
                  "")
              )
              '(#f . #f) 0 1)
          )
        )
      )
    4 18 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4))))

(aforth-syntax-print (define-repeating-code program) 1 1)
