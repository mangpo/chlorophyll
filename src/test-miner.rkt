#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt")

(define program
    ;; core 26
    (aforth 
      ;; list
      (list 
        (vardecl '(0 0 0 0))
        (funcdecl "1if"
          ;; list
          (list 
            (block
              "- 1 + + "
              2 1 (restrict #t #f #f #f)
              "- 1 + + ")
            (-iftf 
              ;; list
              (list 
                (block
                  "2 b! @b 3 b! !b 1 b! @b 2 b! !b "
                  0 0 (restrict #t #f #f #f)
                  "2 b! @b 3 b! !b 1 b! @b 2 b! !b ")
                (block
                  "3 b! @b 1 b! !b 0 b! @b 16 "
                  0 2 (restrict #t #f #f #f)
                  "3 b! @b 1 b! !b 0 b! @b 16 ")
                (block
                  "- 1 + + 0 b! !b "
                  2 0 (restrict #t #f #f #f)
                  "- 1 + + 0 b! !b ")
              )
              ;; list
              (list 
              )
            )
          )
        )
        (funcdecl "leftrotate"
          ;; list
          (list 
            (block
              "0 a! !+ !+ !+ 16 0 b! @b "
              3 2 (restrict #t #f #f #f)
              "0 a! !+ !+ !+ 16 0 b! @b ")
            (funccall "1if")
            (block
              "1 b! @b 16 0 b! @b - 1 + + "
              0 2 (restrict #t #f #f #f)
              "1 b! @b 16 0 b! @b - 1 + + ")
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
                    0 0 (restrict #f #f #f #f)
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
              "drop 2 b! @b 0 b! @b "
              1 2 (restrict #t #f #f #f)
              "drop 2 b! @b 0 b! @b ")
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
                    0 0 (restrict #f #f #f #f)
                    "")
                  ;; list
                  (list 
                    (block
                      "2* "
                      1 1 (restrict #t #f #f #f)
                      "2* ")
                  )
                  #f #f #f)
                (block
                  "dup "
                  1 2 (restrict #t #f #f #f)
                  "dup ")
              )
            )
            (block
              "drop over - and + down b! !b 2 b! @b 16 0 b! @b "
              3 3 (restrict #t #f #f #f)
              "drop over - and + down b! !b 2 b! @b 16 0 b! @b ")
            (block
              "- 1 + + "
              2 1 (restrict #t #f #f #f)
              "- 1 + + ")
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
                    0 0 (restrict #f #f #f #f)
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
              "drop 1 b! @b 0 b! @b "
              1 2 (restrict #t #f #f #f)
              "drop 1 b! @b 0 b! @b ")
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
                    0 0 (restrict #f #f #f #f)
                    "")
                  ;; list
                  (list 
                    (block
                      "2* "
                      1 1 (restrict #t #f #f #f)
                      "2* ")
                  )
                  #f #f #f)
                (block
                  "dup "
                  1 2 (restrict #t #f #f #f)
                  "dup ")
              )
            )
            (block
              "drop over - and + up b! !b "
              3 0 (restrict #t #f #f #f)
              "drop over - and + up b! !b ")
          )
        )
        (funcdecl "csum"
          ;; list
          (list 
            (block
              "down b! @b up b! !b "
              0 0 (restrict #t #f #f #f)
              "down b! @b up b! !b ")
          )
        )
        (funcdecl "sumrotate"
          ;; list
          (list 
            (funccall "csum")
            (block
              "down b! @b up b! @b left b! @b "
              0 3 (restrict #t #f #f #f)
              "down b! @b up b! @b left b! @b ")
            (funccall "leftrotate")
            (funccall "csum")
            (block
              "up b! @b left b! !b "
              0 0 (restrict #t #f #f #f)
              "up b! @b left b! !b ")
          )
        )
        (funcdecl "main"
          ;; list
          (list 
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
          )
        )
      )
    4 18 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4))))

(define-repeating-code program)
