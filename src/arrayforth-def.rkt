#lang racket

(require "header.rkt" 
         "arrayforth.rkt" 
         "arrayforth-print.rkt"
         "arrayforth-miner.rkt")

(define (list->linklist lst)
  (define (inner lst)
    (if (empty? lst)
        (linklist #f #f #f)
        (let* ([rest (inner (cdr lst))]
               [me (linklist #f (car lst) rest)])
          (when rest
            (set-linklist-prev! rest me))
          me)))
  
  (define start (inner lst))
  (define head (linklist #f #f start))
  (set-linklist-prev! start head)
  head)

(define (aforth-linklist ast)
  (cond
   [(list? ast)
    (list->linklist (for/list ([x ast]) (aforth-linklist x)))]

   [(forloop? ast)
    (forloop (forloop-init ast) 
	     (aforth-linklist (forloop-body ast))
	     (forloop-iter ast)
	     (forloop-from ast)
	     (forloop-to ast))]

   [(ift? ast)
    (ift (aforth-linklist (ift-t ast)))]

   [(iftf? ast)
    (iftf (aforth-linklist (iftf-t ast))
          (aforth-linklist (iftf-f ast)))]

   [(-ift? ast)
    (-ift (aforth-linklist (-ift-t ast)))]

   [(-iftf? ast)
    (-iftf (aforth-linklist (-iftf-t ast))
           (aforth-linklist (-iftf-f ast)))]

   [(funcdecl? ast)
    (funcdecl (funcdecl-name ast)
              (aforth-linklist (funcdecl-body ast)))]

   [(aforth? ast)
    (aforth (aforth-linklist (aforth-code ast))
	    (aforth-memsize ast) (aforth-bit ast) (aforth-indexmap ast))]

   [else ast]))

(define count 0)

;; return new name for definition
(define (new-def)
  (set! count (add1 count))
  (format "~arep" count))

(define (insert-definition from to program new-name)
  ;; set head for from
  (define head (linklist #f #f from))
  (set-linklist-prev! from head)
  ;; set tail for to
  (set-linklist-next! to (linklist to #f #f))
  
  #|(pretty-display "FROM!!!!!")
  (pretty-display (send to-string visit (linklist-entry from)))
  (pretty-display "TO!!!!!")
  (pretty-display (send to-string visit (linklist-entry to)))
  (pretty-display (send to-string visit from))|#
  
  ;; insert new funcdecl into program
  (define def-entry (first-funcdecl-linklist (aforth-code program)))
  (define pre-entry (linklist-prev def-entry))
  (define new-entry (linklist pre-entry (funcdecl new-name head) def-entry))
  (set-linklist-next! pre-entry new-entry)
  (set-linklist-prev! def-entry new-entry))

;; mutate program by definig a new definition for the repeated sequences 
;; given as lst argument
(define (make-definition lst program)
  ;; args: list of linklists
  ;; return: true if their entries are the same
  (define (same? x)
    (let ([ele (linklist-entry (car x))])
      (and (not (equal? ele #f))
           (andmap (lambda (a) (aforth-eq? (linklist-entry a) ele))
                   (cdr x)))))
           
  ;; return a pair of
  ;; 1) list of common insts
  ;; 2) list of different list of insts
  (define (common-prefix inst-lists)
    (if (empty? (car inst-lists))
        (cons (list)
              inst-lists)
        (let ([inst (caar inst-lists)])
          (if (andmap (lambda (x) (and (not (empty? x)) (equal? (car x) inst)))
                      (cdr inst-lists))
              ;; if not empty and the first element is the same
              (let ([res (common-prefix (map cdr inst-lists))])
                (cons (cons inst (car res))
                      (cdr res)))
              (cons (list)
                    inst-lists)))))

  ;; update block inside the linklists to the given code
  (define (update linklists inst-lists location)
    (for ([x-linklist linklists]
	  [x-code inst-lists])
	 ;; TODO update in, out, org
      (let* ([b (linklist-entry x-linklist)]
             [org (string-split (block-org b))]
             [inout (estimate-inout x-code)])
        (set-block-body! b x-code)
        (set-block-in! b (car inout))
        (set-block-out! b (cdr inout))
        (if (equal? location `front)
            (set-block-org! b (string-join (take org (length x-code))))
            (set-block-org! b (string-join (drop org (- (length org) (length x-code))))))
        )))

  ;; return the first funcdecl in the given linklist
  (define (first-funcdecl-linklist lst)
    (if (funcdecl? (linklist-entry lst))
	lst
	(first-funcdecl-linklist (linklist-next lst))))


  ;; the first common entries
  (define froms lst)
  ;; the lst common entries
  (define tos lst)
  
  (define ref (linklist-entry (car lst)))

  ;; get previous entries in list of linklists x
  (define (get-from x)
    (if (and (not (aforth-eq? (linklist-entry (car x)) ref))
             (same? x))
	(begin
	  (set! froms x) 
	  (get-from (map linklist-prev x)))
	x))

  ;; get next entries in list of linklists x
  (define (get-to x)
    (if (and (not (aforth-eq? (linklist-entry (car x)) ref))
             (same? x))
	(begin
	  (set! tos x)
	  (get-to (map linklist-next x)))
	x))

  ;; the previous entries from the first common entries
  (define from-diffs (get-from (map linklist-prev lst)))
  ;; the next entries from the last common entries
  (define to-diffs (get-to (map linklist-next lst)))
  
  (define is-gap (not (aforth-eq?
                       (linklist-entry (first froms))
                       (linklist-entry (linklist-next (second tos))))))
  ;(pretty-display (send to-string visit (linklist-entry (first froms))))
  ;(pretty-display (send to-string visit (linklist-entry (linklist-next (second tos)))))
  ;(pretty-display no-space)
                    
  (define prefix #f)
  (define prefix-org #f)
  ;; check that it is a linklist that contains block
  (when (and is-gap
             (andmap (lambda (x) (and (linklist? x) (block? (linklist-entry x)))) 
                     from-diffs))
      ;; if not off the list
      (let* ([first-insts (string-split (block-org (linklist-entry (car from-diffs))))]
             [revs (map (lambda (x)
                          (let* ([insts (block-body (linklist-entry x))]
                                 [insts-list (if (list? insts) insts (string-split insts))])
                            (reverse insts-list)))
                        from-diffs)]
             [pair (common-prefix revs)])
	(update from-diffs (map reverse (cdr pair)) `front)
	(set! prefix (reverse (car pair)))
        (set! prefix-org (drop first-insts (- (length first-insts) (length prefix))))
        ))
  
  (define suffix #f)
  (define suffix-org #f)
  ;; check that it is a linklist that contains block
  (when (and is-gap
             (andmap (lambda (x) (and (linklist? x) (block? (linklist-entry x)))) 
                     to-diffs))
      ;; if not off the list
      (let* ([first-insts (string-split (block-org (linklist-entry (car from-diffs))))]
             [forwards (map (lambda (x)
			     (let* ([insts (block-body (linklist-entry x))]
                                    [insts-list (if (list? insts) insts (string-split insts))])
                               insts-list))
                            to-diffs)]
             [pair (common-prefix forwards)])
        (update to-diffs (cdr pair) `back)
	;; TODO suffix-org
	(set! suffix (car pair))
        (set! suffix-org (take first-insts (length prefix)))
        ))
  
  (define new-name (new-def))
  (define from (car froms))
  (define to (car tos))

  ;; replace common sequences wiht funccalls
  (for ([from froms]
	[to   tos])
    (let* ([from-prev (linklist-prev from)]
           [to-next   (linklist-next to)]
           [new-linklist (linklist from-prev (funccall new-name) to-next)])
      (set-linklist-next! from-prev new-linklist)
      (set-linklist-prev! to-next new-linklist)))
  
  (when prefix
    (let ([inout (estimate-inout prefix)])
      (set! from (linklist #f 
                           (block prefix (car inout) (cdr inout) 
                                  (aforth-memsize program) prefix-org)
                           from))))
  
  (when suffix
    (let* ([inout (estimate-inout suffix)]
           [suffix-linklist (linklist to 
                                      (block suffix (car inout) (cdr inout)
                                             (aforth-memsize program) suffix-org)
                                      #f)])
      (set-linklist-next! to suffix-linklist)
      (set! to suffix-linklist)))
  
  (insert-definition from to program new-name)
  )

(define program
  (aforth 
      (list 
        (vardecl '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
        (funcdecl "sumrotate"
          (list 
            (block
              "down b! @b left b! !b "
              0 0 #t
              "down b! @b left b! !b ")
          )
        )
        (funcdecl "main"
          (list 
            (forloop 
              (block
                "15 "
                0 1 #t
                "15 ")
              (list 
                (block
                  "0 16 b! !b 1 17 b! !b "
                  0 0 #t
                  "0 16 b! !b 1 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 0 16)
                (block
                  "1 16 b! !b 5 17 b! !b "
                  0 0 #t
                  "1 16 b! !b 5 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 16 32)
                (block
                  "5 16 b! !b 3 17 b! !b "
                  0 0 #t
                  "5 16 b! !b 3 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 32 48)
                (block
                  "0 16 b! !b 7 17 b! !b "
                  0 0 #t
                  "0 16 b! !b 7 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 48 64)
              )
              '(#f . #f) 0 16)
          )
        )
      )
    20 18 #hash((6 . 20) (0 . 0) (2 . 16) (3 . 17) (4 . 18) (5 . 19))))

(define program2
  (aforth 
      (list 
        (funcdecl "sumrotate"
          (list 
            (block
              "down b! @b left b! !b "
              0 0 #t
              "down b! @b left b! !b ")
          )
        )
        (funcdecl "main"
          (list 
            (forloop 
              (block
                "15 "
                0 1 #t
                "15 ")
              (list 
                (block
                  "1 17 b! !b "
                  0 0 #t
                  "1 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 0 16)
                (funccall "sumrotate")
                (block
                  "dup dup or 2 17 b! !b "
                  0 0 #t
                  "dup dup or 2 17 b! !b ")
                (forloop 
                  (block
                    "15 "
                    0 1 #t
                    "15 ")
                  (list 
                    (block
                      "16 b! @b a! @ left b! !b "
                      0 0 #t
                      "16 b! @b a! @ left b! !b ")
                    (funccall "sumrotate")
                    (block
                      "16 b! @b 17 b! @b + 15 and 16 b! !b "
                      0 0 #t
                      "16 b! @b 17 b! @b + 15 and 16 b! !b ")
                  )
                  '(#f . #f) 16 32)
                (funccall "sumrotate")
                (block
                  "dup dup or pop "
                  0 0 #t
                  "dup dup or pop ")
                (block
                  "dup dup or 2 17 b! !b "
                  0 0 #t
                  "dup dup or 2 17 b! !b ")
              )
              '(#f . #f) 0 16)
          )
        )
      )
    20 18 #hash((6 . 20) (0 . 0) (2 . 16) (3 . 17) (4 . 18) (5 . 19))))

(define (define-repeating-seq program)
  (define (get-linklist ll index)
    (define entry (linklist-entry ll))
    (define insts (string-join (block-body insts)))
    (define org (block-org insts))
    
    (if (> (length insts) index)
        (values ll index insts org)
        (get-linklist (linklist-next ll) (- index (length insts)))))
    
  (define (split-linklist ll index code org)
    (let* ([insts (substring code 0 index)]
           [org (substring org 0 index)]
           [inout (estimate-inout insts)]
           [entry (linklist-entry ll)]
           [snd-insts (substring code index)]
           [snd-org (substring org index)]
           [snd-inout (estimate-inout snd-insts)]
           [new-linklist (linklist (linklist-prev ll)
                                   (if org
                                       (block insts (car inout) (cdr inout)
                                              (aforth-memsize program) 
                                              org)
                                       (funccall insts))
                                   ll)])
      (set-block-body! entry snd-insts)
      (set-block-org! entry snd-org)
      (set-block-in! entry (car snd-inout))
      (set-block-out! entry (cdr snd-inout))
      (set-linklist-next! (linklist-prev ll) new-linklist)
      (set-linklist-prev! ll new-linklist)))
  
  (define (replace-with ll from to func)
    (define-values (ll-from index-from code-from org-from) (get-linklist ll from))
    (define-values (ll-to index-to code-to org-to)         (get-linklist ll (sub1 to)))
    (set! index-to (add1 index-to))
    
    (when (> index-start 0)
      (split-linklist ll-from index-from code-from org-from index-from))
    
    (when (< index-to (length code-to))
      (split-linklist ll-to index-to code-to org-to index-to)
      (set! to (linklist-next ro))
  
    (define from-prev (linklist-prev from))
    (define to-next (linklist-next to))
    
    (define new-linklist (linklist from-prev func to-next))
    (set-linklist-next! from-prev new-linklist)
    (set-linklist-prev! to-next new-linklist)
    (values from to)
    ))

  (define (define-and-replace locations)
    (define new-name (new-def))
    (define-values (from to)
      (for/first ([location locations])
        (replace-with (car location) (cadr location) (cddr location) (funccall new-name))))
    
    (insert-definition from to program new-name))
  
  (define linklist-program (aforth-linklist program))
  (define same-structures (send (new structure-extractor%) visit linklist-program))
  (make-definition same-structures linklist-program)
  (aforth-struct-print linklist-program)
  
  (define-values (seqs max-len) (send (new sequence-miner%) visit linklist-program))
  (define subseqs (sort-subsequence seqs 6 max-len))
  (pretty-display seqs)
  
  (let* ([matcher (new sequence-matcher% 
                      [exp (regexp "dup dup or")])])
         ;; [exp (regexp (string-join (car subseqs)))])]
    (let ([locations (send matcher visit linklist-program)])
      (when (> (length locations) 1)
        (define-and-replace locations)))
    ))

(define-repeating-seq program2)
