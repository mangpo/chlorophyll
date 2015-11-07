#lang s-exp rosette

(require "../arrayforth.rkt" "simulator.rkt")

(define code
      (list 
        (iftf 
          ;; list
          (list 
            (block
              "drop "
              1 0 (restrict #t #f #f #f) #f
              "drop " #;#t)
            (block
              "131072 io b! !b "
              0 0 (restrict #t #f #f #f) "io"
              "131072 io b! !b " #;#f)
          )
          ;; list
          (list 
            (block
              "drop "
              1 0 (restrict #t #f #f #f) #f
              "drop " #;())
            (block
              "196608 io b! !b "
              0 0 (restrict #t #f #f #f) "io"
              "196608 io b! !b " #;())
          )
        )
      ))

(define state-i (get-progstate 0))
(define state-f (analyze-reg-b code state-i 108))
(aforth-struct-print code)
