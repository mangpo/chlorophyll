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
    ;; core 12
    (aforth 
      ;; linklist
      (list 
        (vardecl '(0 32867 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (funcdecl "sumrotate"
          ;; linklist
          (list 
            (block
              "0 a! !+ "
              1 0 (restrict #t #f #f #f)
              "0 a! !+ ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
          )
        )
        (funcdecl "57rep"
          ;; linklist
          (list 
            (block
              ""
              0 0 (restrict #t #f #f #f)
              "")
            (block
              "4 b! !b "
              1 0 (restrict #t #f #f #f)
              "18 b! !b ")
            (forloop 
              (block
                "15 "
                0 1 (restrict #t #f #f #f)
                "15 ")
              ;; linklist
              (list 
                (block
                  "3 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "17 b! @b ")
                (block
                  "1 + b! @b "
                  1 1 (restrict #t #f #f #f)
                  "1 + b! @b ")
                (funccall "sumrotate")
                (block
                  "3 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "17 b! @b ")
                (block
                  "4 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "18 b! @b ")
                (block
                  "+ "
                  2 1 (restrict #t #f #f #f)
                  "+ ")
                (block
                  "15 "
                  0 1 (restrict #t #f #f #f)
                  "15 ")
                (block
                  "and "
                  2 1 (restrict #t #f #f #f)
                  "and ")
                (block
                  "3 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "17 b! !b ")
              )
              '(#f . #f) 48 64)
          )
        )
      )
    5 18 #hash((0 . 0) (1 . 1) (3 . 17) (4 . 18) (5 . 19)))
    #f))

;(define-repeating-code program)
(aforth-syntax-print (superoptimize program "foo" 1 1) 1 1)
#|(program-diff? "down b! @b right b! !b down nop b! @b" 
               "277 b! @b 469 a! ! @b nop"
               1
               (constraint s t memory) 18)|#