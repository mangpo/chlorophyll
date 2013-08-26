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
    ;; core 23
    (aforth 
      ;; linklist
      (list 
        (funcdecl "68rep"
          ;; linklist
          (list 
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
              ""
              0 0 (restrict #t #f #f #f)
              "")
          )
        )
        (funcdecl "leftrotate"
          ;; linklist
          (list 
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "1 "
              0 1 (restrict #t #f #f #f)
              "1 ")
            (funccall "69rep")
            (block
              " b! !b"
              2 0 (restrict #t #f #f #f)
              " b! !b")
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "0 b! !b "
              1 0 (restrict #t #f #f #f)
              "0 b! !b ")
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "1 "
              0 1 (restrict #t #f #f #f)
              "1")
            (funccall "68rep")
            (block
              "down "
              0 1 (restrict #t #f #f #f)
              "down ")
            (funccall "69rep")
            (funccall "68rep")
            (block
              "up b! !b "
              1 0 (restrict #t #f #f #f)
              "up b! !b")
          )
        )
        (funcdecl "csum"
          ;; linklist
          (list 
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "up b! !b "
              1 0 (restrict #t #f #f #f)
              "up b! !b ")
          )
        )
        (funcdecl "sumrotate"
          ;; linklist
          (list 
            (funccall "csum")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
            (block
              "up b! @b "
              0 1 (restrict #t #f #f #f)
              "up b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
            (funccall "leftrotate")
            (funccall "csum")
          )
        )
        (funcdecl "main"
          ;; linklist
          (list 
            (forloop 
              (block
                "63 "
                0 1 (restrict #t #t #f #t)
                "63 ")
              ;; linklist
              (list 
                (funccall "sumrotate")
              )
              '(#f . #f) 0 64)
          )
        )
      )
    3 18 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3)))
    #f))

;(define-repeating-code program)
(aforth-syntax-print (superoptimize program "foo" 1 1) 1 1)
#|(program-diff? "down b! @b right b! !b down nop b! @b" 
               "277 b! @b 469 a! ! @b nop"
               1
               (constraint s t memory) 18)|#