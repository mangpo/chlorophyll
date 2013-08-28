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
    ;; core 21
    (aforth 
      ;; linklist
      (list 
        (vardecl '(6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21))
        (funcdecl "sumrotate"
          ;; linklist
          (list 
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "left b! !b "
              1 0 (restrict #t #f #f #f)
              "left b! !b ")
          )
        )
        (funcdecl "main"
          ;; linklist
          (list 
            (forloop 
              (block
                "47 "
                0 1 (restrict #t #t #f #t)
                "47 ")
              ;; linklist
              (list 
                (funccall "sumrotate")
              )
              '(#f . #f) 0 48)
            (forloop 
              (block
                "0 a! 15 "
                0 1 (restrict #t #t #f #f)
                "0 a! 15 ")
              ;; linklist
              (list 
                (block
                  "@+ "
                  1 1 (restrict #t #t #f #f)
                  "@+ ")
                (block
                  "right b! !b "
                  1 0 (restrict #t #t #f #f)
                  "right b! !b ")
                (block
                  "a push "
                  0 0 (restrict #t #t #f #t)
                  "a push ")
                (funccall "sumrotate")
                (block
                  "pop a! "
                  0 0 (restrict #t #t #f #f)
                  "pop a! ")
              )
              '((0 . opt) 0 . opt) 0 16)
          )
        )
      )
    2 18 #hash((0 . 0) (2 . 16)))
    ;; core 22
    (aforth 
      ;; linklist
      (list 
        (vardecl '(0 0 0 0))
        (funcdecl "64rep"
          ;; linklist
          (list 
            (block
              "b! !b"
              2 0 (restrict #t #f #f #f)
              "b! !b")
            (block
              "down b! @b "
              0 1 (restrict #t #f #f #f)
              "down b! @b ")
            (block
              "right"
              0 1 (restrict #t #f #f #f)
              "right")
          )
        )
        (funcdecl "1if"
          ;; linklist
          (list 
            (block
              "- 1 + + "
              2 1 (restrict #t #f #f #f)
              "- 1 + + ")
            (-iftf 
              ;; linklist
              (list 
                (block
                  "2 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "2 b! @b ")
                (block
                  "3 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "3 b! !b ")
                (block
                  "1 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "1 b! @b ")
                (block
                  "2 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "2 b! !b ")
                (block
                  "3 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "3 b! @b ")
                (block
                  "1 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "1 b! !b ")
                (block
                  "0 b! @b "
                  0 1 (restrict #t #f #f #f)
                  "0 b! @b ")
                (block
                  "16 "
                  0 1 (restrict #t #f #f #f)
                  "16 ")
                (block
                  "- 1 + + "
                  2 1 (restrict #t #f #f #f)
                  "- 1 + + ")
                (block
                  "0 b! !b "
                  1 0 (restrict #t #f #f #f)
                  "0 b! !b ")
              )
              ;; linklist
              (list 
              )
            )
          )
        )
        (funcdecl "leftrotate"
          ;; linklist
          (list 
            (block
              "0 a! !+ !+ !+ "
              3 0 (restrict #t #f #f #f)
              "0 a! !+ !+ !+ ")
            (block
              "16 "
              0 1 (restrict #t #f #f #f)
              "16 ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
            (funccall "1if")
            (block
              "2 b! @b "
              0 1 (restrict #t #f #f #f)
              "2 b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
            (block
              "1 b! @b "
              0 1 (restrict #t #f #f #f)
              "1 b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
            (block
              "2 b! @b "
              0 1 (restrict #t #f #f #f)
              "2 b! @b ")
            (block
              "down b! !b "
              1 0 (restrict #t #f #f #f)
              "down b! !b ")
            (block
              "1 b! @b "
              0 1 (restrict #t #f #f #f)
              "1 b! @b ")
            (block
              "down b! !b "
              1 0 (restrict #t #f #f #f)
              "down b! !b ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
            (block
              "right b! !b "
              1 0 (restrict #t #f #f #f)
              "right b! !b ")
            (block
              "0 b! @b "
              0 1 (restrict #t #f #f #f)
              "0 b! @b ")
            (block
              "down "
              0 1 (restrict #t #f #f #f)
              "down ")
            (funccall "64rep")
            (funccall "64rep")
            (block
              " b! !b"
              2 0 (restrict #t #f #f #f)
              " b! !b")
          )
        )
        (funcdecl "sumrotate"
          ;; linklist
          (list 
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "right b! @b "
              0 1 (restrict #t #f #f #f)
              "right b! @b ")
            (block
              "left b! @b "
              0 1 (restrict #t #f #f #f)
              "left b! @b ")
            (funccall "leftrotate")
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
    4 18 #f)
    #f))

;(define-repeating-code program)
(aforth-syntax-print (superoptimize program "foo" 2 1) 2 1)
#|(program-diff? "down b! @b right b! !b down nop b! @b" 
               "277 b! @b 469 a! ! @b nop"
               1
               (constraint s t memory) 18)|#