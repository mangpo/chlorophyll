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
  ;; linklist
  (list 
    (vardecl '(0 0 0 0 0 0))
    (funcdecl "53rep"
      ;; linklist
      (list 
        (block
          "right b! @b "
          0 1 (restrict #t #f #f #f) "right"
          "right b! @b ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "0 b! @b dup"
          0 2 (restrict #t #f #f #f) #f
          "0 b! @b dup")
      )
    )
    (funcdecl "leftrotate"
      ;; linklist
      (list 
        (block
          "left b! @b "
          0 1 (restrict #t #f #f #f) "left"
          "left b! @b ")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "0 b! @b over"
          1 3 (restrict #t #f #f #f) #f
          "0 b! @b over")
      )
    ))
6 17 #hash((0 . 0) (1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5) (6 . 6)))))
(set! program (define-repeating-codes program 1 1))
(aforth-struct-print program)
(define real-opts (superoptimize program "test" 1 1))
(aforth-syntax-print real-opts 1 1)