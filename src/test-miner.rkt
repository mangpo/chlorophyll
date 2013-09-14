#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt"
         "arrayforth-optimize.rkt"
         "../../forth-interpreter/machine/cegis.rkt" 
         "../../forth-interpreter/machine/state.rkt" 
         "../../forth-interpreter/machine/track-constant.rkt"
         )

(define program
  (vector
    ;; core 14
    (aforth 
      ;; list
      (list 
        (vardecl '(0 0 0))
        (funcdecl "leftrotate"
          ;; list
          (list 
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "1 b! !b "
              1 0 (restrict #t #f #f #f)
              "1 b! !b ")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "2 b! !b "
              1 0 (restrict #t #f #f #f)
              "2 b! !b ")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "up b! !b "
              1 0 (restrict #t #f #f #f)
              "up b! !b ")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "up b! !b "
              1 0 (restrict #t #f #f #f)
              "up b! !b ")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "0 b! !b "
              1 0 (restrict #t #f #f #f)
              "0 b! !b ")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "up b! !b "
              1 0 (restrict #t #f #f #f)
              "up b! !b ")
            (block
              "up b! @b "
              0 1 (restrict #t #f #f #f)
              "up b! @b ")
            (block
              "1 b! @b "
              0 1 (restrict #t #f #f #f)
              "1 b! @b ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
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
              "drop "
              1 0 (restrict #t #f #f #f)
              "drop ")
            (block
              "over - and + "
              2 1 (restrict #t #f #f #f)
              "over - and + ")
            (block
              "65535 "
              0 1 (restrict #t #f #f #f)
              "65535 ")
            (block
              "and "
              2 1 (restrict #t #f #f #f)
              "and ")
            (block
              "left b! !b "
              1 0 (restrict #t #f #f #f)
              "left b! !b ")
            (block
              "up b! @b "
              0 1 (restrict #t #f #f #f)
              "up b! @b ")
            (block
              "2 b! @b "
              0 1 (restrict #t #f #f #f)
              "2 b! @b ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
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
              "drop "
              1 0 (restrict #t #f #f #f)
              "drop ")
            (block
              "over - and + "
              2 1 (restrict #t #f #f #f)
              "over - and + ")
            (block
              "65535 "
              0 1 (restrict #t #f #f #f)
              "65535 ")
            (block
              "and "
              2 1 (restrict #t #f #f #f)
              "and ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
          )
        )
      )
    3 18 #f)
    #f))

(aforth-struct-print (define-repeating-codes program 1 1))
;(aforth-syntax-print (superoptimize program "foo" 2 1) 2 1)
#|(program-diff? "down b! @b right b! !b down nop b! @b" 
               "277 b! @b 469 a! ! @b nop"
               1
               (constraint s t memory) 18)|#