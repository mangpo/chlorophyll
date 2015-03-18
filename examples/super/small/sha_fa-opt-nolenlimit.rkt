(aforth 
  ;; linklist
  (list 
    (vardecl '(0 0))
    (funcdecl "f1"
      ;; linklist
      (list 
        (block
          "dup push or nop and pop or"
          3 1 (restrict #f #f #f #f) #f
          "0 a! !+ !+ push pop dup 1 b! @b and over - 0 b! @b and or push drop pop ")
      )
    '(#f #f #f))
  )
2 18 #f)
