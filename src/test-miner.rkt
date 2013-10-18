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
        (vardecl '(5 4 1 3 1 0 1 0 9 4 0 0))
        (funcdecl "main"
          ;; list
          (list 
            (forloop 
              (block
                "0 10 b! !b 0 "
                0 1 (restrict #t #f #f #f) #f
                "0 10 b! !b 0 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "4 11 b! !b 0 "
                    0 1 (restrict #t #f #f #f) #f
                    "4 11 b! !b 0 ")
                  ;; list
                  (list 
                    (block
                      "10 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "10 b! @b ")
                    (block
                      "11 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "11 b! @b ")
                    (block
                      "+ "
                      2 1 (restrict #t #f #f #f) #f
                      "+ ")
                    (block
                      "-4 + b! @b "
                      1 1 (restrict #t #f #f #f) #f
                      "-4 + b! @b ")
                    (block
                      "right b! !b "
                      1 0 (restrict #t #f #f #f) "right"
                      "right b! !b ")
                    (block
                      "11 b! @b 1 + !b "
                      0 0 (restrict #t #f #f #f) #f
                      "11 b! @b 1 + !b ")
                  )
                  '(11 . 11) 4 5)
                (block
                  "10 b! @b 1 + !b "
                  0 0 (restrict #t #f #f #f) #f
                  "10 b! @b 1 + !b ")
              )
              '(10 . 10) 0 1)
            (forloop 
              (block
                "0 10 b! !b 0 "
                0 1 (restrict #t #f #f #f) #f
                "0 10 b! !b 0 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "3 11 b! !b 1 "
                    0 1 (restrict #t #t #f #t) #f
                    "3 11 b! !b 1 ")
                  ;; list
                  (list 
                    (block
                      "10 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "10 b! @b ")
                    (block
                      "11 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "11 b! @b ")
                    (block
                      "+ "
                      2 1 (restrict #t #f #f #f) #f
                      "+ ")
                    (block
                      "-3 + b! @b "
                      1 1 (restrict #t #f #f #f) #f
                      "-3 + b! @b ")
                    (block
                      "right b! !b "
                      1 0 (restrict #t #f #f #f) "right"
                      "right b! !b ")
                    (block
                      "11 b! @b 1 + !b "
                      0 0 (restrict #t #f #f #f) #f
                      "11 b! @b 1 + !b ")
                  )
                  '(11 . 11) 3 5)
                (block
                  "10 b! @b 1 + !b "
                  0 0 (restrict #t #f #f #f) #f
                  "10 b! @b 1 + !b ")
              )
              '(10 . 10) 0 1)
            (forloop 
              (block
                "0 10 b! !b 0 "
                0 1 (restrict #t #f #f #f) #f
                "0 10 b! !b 0 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "2 11 b! !b 2 "
                    0 1 (restrict #t #t #f #t) #f
                    "2 11 b! !b 2 ")
                  ;; list
                  (list 
                    (block
                      "10 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "10 b! @b ")
                    (block
                      "11 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "11 b! @b ")
                    (block
                      "+ "
                      2 1 (restrict #t #f #f #f) #f
                      "+ ")
                    (block
                      "-2 + b! @b "
                      1 1 (restrict #t #f #f #f) #f
                      "-2 + b! @b ")
                    (block
                      "right b! !b "
                      1 0 (restrict #t #f #f #f) "right"
                      "right b! !b ")
                    (block
                      "11 b! @b 1 + !b "
                      0 0 (restrict #t #f #f #f) #f
                      "11 b! @b 1 + !b ")
                  )
                  '(11 . 11) 2 5)
                (block
                  "10 b! @b 1 + !b "
                  0 0 (restrict #t #f #f #f) #f
                  "10 b! @b 1 + !b ")
              )
              '(10 . 10) 0 1)
            (forloop 
              (block
                "0 10 b! !b 0 "
                0 1 (restrict #t #f #f #f) #f
                "0 10 b! !b 0 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "1 11 b! !b 3 "
                    0 1 (restrict #t #t #f #t) #f
                    "1 11 b! !b 3 ")
                  ;; list
                  (list 
                    (block
                      "10 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "10 b! @b ")
                    (block
                      "11 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "11 b! @b ")
                    (block
                      "+ "
                      2 1 (restrict #t #f #f #f) #f
                      "+ ")
                    (block
                      "-1 + b! @b "
                      1 1 (restrict #t #f #f #f) #f
                      "-1 + b! @b ")
                    (block
                      "right b! !b "
                      1 0 (restrict #t #f #f #f) "right"
                      "right b! !b ")
                    (block
                      "11 b! @b 1 + !b "
                      0 0 (restrict #t #f #f #f) #f
                      "11 b! @b 1 + !b ")
                  )
                  '(11 . 11) 1 5)
                (block
                  "10 b! @b 1 + !b "
                  0 0 (restrict #t #f #f #f) #f
                  "10 b! @b 1 + !b ")
              )
              '(10 . 10) 0 1)
            (forloop 
              (block
                "0 10 b! !b 5 "
                0 1 (restrict #t #f #f #f) #f
                "0 10 b! !b 5 ")
              ;; list
              (list 
                (forloop 
                  (block
                    "0 11 b! !b 4 "
                    0 1 (restrict #t #t #f #t) #f
                    "0 11 b! !b 4 ")
                  ;; list
                  (list 
                    (block
                      "10 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "10 b! @b ")
                    (block
                      "11 b! @b "
                      0 1 (restrict #t #f #f #f) #f
                      "11 b! @b ")
                    (block
                      "+ "
                      2 1 (restrict #t #f #f #f) #f
                      "+ ")
                    (block
                      "b! @b "
                      1 1 (restrict #t #f #f #f) #f
                      "b! @b ")
                    (block
                      "down b! !b "
                      1 0 (restrict #t #f #f #f) "down"
                      "down b! !b ")
                    (block
                      "11 b! @b 1 + !b "
                      0 0 (restrict #t #f #f #f) #f
                      "11 b! @b 1 + !b ")
                  )
                  '(11 . 11) 0 5)
                (block
                  "10 b! @b 1 + !b "
                  0 0 (restrict #t #f #f #f) #f
                  "10 b! @b 1 + !b ")
              )
              '(10 . 10) 0 6)
          )
        )
      )
    12 2 #f)

))
(set! program (define-repeating-codes program 1 1))
;(aforth-struct-print program)
(aforth-syntax-print program 1 1)
;(define real-opts (superoptimize program "test" 1 1))
;(aforth-syntax-print real-opts 1 1)