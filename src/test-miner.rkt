#lang racket

(require "arrayforth.rkt" "arrayforth-def.rkt" 
	 "arrayforth-print.rkt"
         "arrayforth-optimize.rkt"
         )

(define program
  (vector
(aforth 
  ;; linklist
  (list 
    (vardecl '())
    (funcdecl "osc"
      ;; linklist
      (list 
        (forloop 
          ;; linklist
          (list 
            (block
              "4999 "
              0 1 (restrict #t #f #f #f) #f
              "4999 ")
          )
          ;; linklist
          (list 
            (block
              "io b! 196608 !b "
              0 0 (restrict #t #f #f #f) "io"
              "io b! 196608 !b ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (forloop 
          ;; linklist
          (list 
          )
          ;; linklist
          (list 
          )
          #f #f #f)
            (block
              "io b! 131072 !b "
              0 0 (restrict #t #f #f #f) "io"
              "io b! 131072 !b ")
        (block
          "dup "
          0 1 (restrict #t #f #f #f) #f
          "dup ")
        (forloop 
          ;; linklist
          (list 
          )
          ;; linklist
          (list 
          )
          #f #f #f)
          )
          '(#f . #f) 0 5000)
      )
    #f)
    (funcdecl "main"
      ;; linklist
      (list 
        (funccall "osc")
      )
    #f)
  )
0 18 #f #f #f #f)
))
(set! program (define-repeating-codes program 1 1))
(aforth-syntax-print program 1 1)
(aforth-struct-print program)
;(define real-opts (superoptimize program "test" 1 1))
;(aforth-syntax-print real-opts 1 1)
