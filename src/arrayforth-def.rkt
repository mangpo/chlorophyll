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


;; return the first funcdecl in the given linklist
(define (first-funcdecl-linklist lst)
  (if (funcdecl? (linklist-entry lst))
      lst
      (first-funcdecl-linklist (linklist-next lst))))

(define count 0)

;; Return new name for definition.
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

;; Mutate program by definig a new definition for the repeated structure. 
;; lst is a list of linklists that point to the starting of the structure
;; at multiple places in the program.
(define (extract-structure lst program)

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




  ;; the first common entries
  (define froms lst)
  ;; the last common entries
  (define tos lst)
  
  (define ref (linklist-entry (car lst)))

  ;; get previous entries in list of linklists x
  (define (get-from x)
    (if (and (not (aforth-eq? (linklist-entry (car x)) ref))
	     ;; compare to ref to ensure we don't extract overlapping chunk
             (same? x))
	(begin
	  (set! froms x) 
	  (get-from (map linklist-prev x)))
	x))

  ;; get next entries in list of linklists x
  (define (get-to x)
    (if (and (not (aforth-eq? (linklist-entry (car x)) ref))
	     ;; compare to ref to ensure we don't extract overlapping chunk
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

(define (extract-all-structure linklist-program)
  (define same-structures (send (new structure-extractor%) visit linklist-program))
  (when same-structures
	(extract-structure same-structures linklist-program)
	;; call itself until there is no more repeated structure found.
	(extract-all-structure linklist-program)))

(define program
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
              )
              '(#f . #f) 0 16)
          )
        )
      )
    20 18 #hash((6 . 20) (0 . 0) (2 . 16) (3 . 17) (4 . 18) (5 . 19))))

(define (extract-all-sequence linklist-program)
  
  (define (get-linklist ll index)
    (define entry (linklist-entry ll))
    (define insts 
      (if (block? entry)
          (if (string? (block-body entry))
              (string-trim (block-body entry))
              (string-join (block-body entry)))
          (funccall-name entry)))
    (define org 
      (if (block? entry)
          (if (string? (block-org entry))
              (string-trim (block-org entry))
              (string-join (block-org entry)))
          (funccall-name entry)))
    
    (if (> (string-length insts) index)
        (values ll index insts org)
        (get-linklist (linklist-next ll) (- index (string-length insts) 1)))) ;; minus 1 for space
    
  ;; Split linklist ll into 2 linklists by inserting a new linklist
  ;; after the splitted one.
  (define (split-linklist ll index code org)
    (pretty-display `(split-linklist ,(substring code 0 index) ,(substring code index)))
    (pretty-display `(org ,org))
    (let* ([insts (substring code 0 index)]
           [fst-org (substring org 0 index)]
           [inout (estimate-inout insts)]
           [entry (linklist-entry ll)]
           [snd-insts (substring code index)]
           [snd-org (substring org index)]
           [snd-inout (estimate-inout snd-insts)]
           [new-linklist (linklist ll
                                   (if snd-org
                                       (block snd-insts (car snd-inout) (cdr snd-inout)
                                              (aforth-memsize program) 
                                              snd-org)
                                       (funccall snd-insts))
                                   (linklist-next ll))])
      (set-block-body! entry insts)
      (set-block-org! entry fst-org)
      (set-block-in! entry (car inout))
      (set-block-out! entry (cdr inout))
      (set-linklist-prev! (linklist-next ll) new-linklist)
      (set-linklist-next! ll new-linklist)))
  
  (define (replace-with head func exp)
    (define ll (linklist-next head))
    (pretty-display "LOCATION")
    (aforth-struct-print ll)
    (define pair (first-location exp ll))
    (define from (car pair))
    (define to (cdr pair))

    (aforth-struct-print ll)
    
    (define-values (ll-from index-from code-from org-from) (get-linklist ll from))
    (define-values (ll-to index-to code-to org-to)         (get-linklist ll (sub1 to)))
    (set! index-to (add1 index-to))
    
    (when (> index-from 0)
      (pretty-display "(> index-from)")
      (split-linklist ll-from index-from code-from org-from)
      (when (equal? ll-from ll-to)
        (set! ll-to (linklist-next ll-from)))
      (set! ll-from (linklist-next ll-from))
      )
    
    (when (< index-to (string-length code-to))
      (pretty-display "(< index-to)")
      (split-linklist ll-to index-to code-to org-to)
      )
  
    (define from-prev (linklist-prev ll-from))
    (define to-next (linklist-next ll-to))
    (pretty-display "TO-NEXT")
    (aforth-struct-print to-next)
    
    (pretty-display ">>> FROM-PREV before")
    (aforth-struct-print from-prev)
    (pretty-display "<<< FROM-PREV before")
    
    (define new-linklist (linklist from-prev func to-next))
    (pretty-display ">>> NEW")
    (aforth-struct-print new-linklist)
    (pretty-display "<<< NEW")
    (set-linklist-next! from-prev new-linklist)
    (set-linklist-prev! to-next new-linklist)
    
    (pretty-display ">>> FROM-PREV after")
    (aforth-struct-print from-prev)
    (pretty-display "<<< FROM-PREV after")
    
    (cons ll-from ll-to)
    )

  (define (define-and-replace locations exp)
    (define new-name (new-def))
    (pretty-display locations)
    (define res
      (car
       (for/list ([location locations])
         (replace-with location (funccall new-name) exp))))
    
    (insert-definition (car res) (cdr res) linklist-program new-name))
  
  (define-values (seqs max-len) (send (new sequence-miner%) visit linklist-program))
  (define subseqs (sort-subsequence seqs 3 max-len))
  (pretty-display seqs)
  
  (for ([subseq subseqs])
       (let* ([str (string-join subseq)]
              [reformatted (string-replace (string-replace str "+" "\\+") "*" "\\*")]
              [exp (regexp reformatted)]
              [matcher (new sequence-matcher% [exp exp])]
              [locations (send matcher visit linklist-program)])
         (pretty-display (format "STRING: ~a" reformatted))
         (when (> (length locations) 1)
               (define-and-replace locations exp)))
       )
  
  ;(aforth-struct-print linklist-program)
  )

(define (reorder-definition program)
  (define dependency-collector (new dependencey-collector%))

  (define (get-funcdecl lst)
    (define entry (linklist-entry lst))
    (if (funcdecl? entry)
	(cons (cons entry (send dependency-collector visit entry))
	      (get-funcdecl (linklist-next lst)))
	(list)))

  ;; Return a topo sorted list of funcdecls
  (define (topo-sort lst)
    (if (empty? lst)
	lst
	(let* ([fst (findf (lambda (pair) (set-empty? (cdr pair))) lst)]
	       [func (car fst)]
	       [name (funcdecl-name func)]
	       [rst (remove fst lst 
			    (lambda (x y) (equal? (funcdecl-name (car x)) (funcdecl-name (car y)))))]
	       [updated (map (lambda (pair) (cons (car pair)
						  (set-remove (cdr pair) name)))
			     rst)])
          (cons (car fst) (topo-sort updated)))))
	  

  (define start (first-funcdecl-linklist (aforth-code program)))
  (define funcdecls (topo-sort (get-funcdecl start)))
  (define new-start (list->linklist funcdecls))
  (set-linklist-next! (linklist-prev start) new-start)
  (set-linklist-prev! new-start (linklist-prev start))
)

(define (define-repeating-code program)
  (define linklist-program (aforth-linklist program))
  (extract-all-structure linklist-program)
  (extract-all-sequence linklist-program)
  (reorder-definition linklist-program)
  (aforth-struct-print linklist-program)
)

(define-repeating-code program)
