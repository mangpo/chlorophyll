#lang s-exp rosette

(require "../arrayforth.rkt" "simulator.rkt")

(define code
(aforth 
  ;; linklist
  (list 
    (vardecl '(0 -281 5203 -42329 37407 0))
    (funcdecl "1if"
      ;; linklist
      (list 
        (-iftf 
          ;; linklist
          (list 
            (block
             "0 b! "
             0 0 (restrict #t #f #f #f) #f
             "0 b! ")
          )
          ;; linklist
          (list 
            (block
             "0 b! "
             0 0 (restrict #t #f #f #f) #f
             "0 b! ")
          )
        )
        (funccall "1if")
      )
    #f)
    (funcdecl "main"
      ;; linklist
      (list
        (funccall "--u/mod")
        (block
         "0 b!"
         0 0 (restrict #t #f #f #f) "0"
         "0 b!")
        (ift 
          ;; linklist
          (list 
            (forloop 
              ;; linklist
              (list 
              )
              ;; linklist
              (list 
                (block
                  "2* "
                  1 1 (restrict #t #f #f #f) #f
                  "2* ")
              )
              #f #f #f)
            (block
              "dup "
              1 2 (restrict #t #f #f #f) #f
              "dup ")
          )
        )

        (funccall "1if")
        ;(funccall "*.")
        (block
          "0 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "0 b! !b ")
        (block
          "3 b! !b "
          1 0 (restrict #t #f #f #f) #f
          "5 b! !b ")
        (forloop 
          ;; linklist
          (list 
            (block
              "1 a! 3 "
              0 1 (restrict #t #t #f #f) #f
              "1 a! 3 ")
          )
          ;; linklist
          (list 
            (block
              "3 b! @b "
              0 1 (restrict #t #t #f #f) #f
              "5 b! @b ")
            (block
              "0 b! @b "
              0 1 (restrict #t #t #f #f) #f
              "0 b! @b ")
            ;(funccall "*.")
            (block
              "@+ "
              1 1 (restrict #t #t #f #f) #f
              "@+ ")
            (block
              "3 b! !b "
              1 0 (restrict #t #t #f #f) #f
              "5 b! !b ")
          )
          '((1 . opt) 1 . opt) 0 4)
        (block
          "3 b! @b "
          0 1 (restrict #t #f #f #f) #f
          "5 b! @b ")
        ;(funccall "*.")
        (block
          "up b! !b "
          1 0 (restrict #t #f #f #f) "up"
          "up b! !b ")
        ;(funccall "1if")
      )
    #f)
  )
4 18 #hash((0 . 0) (1 . 1) (3 . 5) (4 . 6)))
)

(define state-i (get-progstate 4))
(define state-f (analyze-reg-b code state-i 0))
(aforth-struct-print code)
