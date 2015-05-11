#lang racket

(require "header.rkt" 
         "arrayforth.rkt" 
         "arrayforth-print.rkt"
         "arrayforth-miner.rkt")

(provide define-repeating-code define-repeating-codes aforth-linklist)

(define debug #f)

(define (list->linklist lst)
  (define (copy x)
    (if (block? x)
        (struct-copy block x)
        x))

  (define (inner lst)
    (if (empty? lst)
        (linklist #f #f #f)
        (let* ([rest (inner (cdr lst))]
               [me (linklist #f (copy (car lst)) rest)])
          (when rest
            (set-linklist-prev! rest me))
          me)))
  
  (define non-empty (filter (lambda (x) (not (empty-block? x))) lst))
  (define start (inner non-empty))
  (define head (linklist #f #f start))
  (set-linklist-prev! start head)
  head)

(define (linklist->list lst)
  (define (inner lst)
    (if (linklist-entry lst)
	(cons (linklist-entry lst) (inner (linklist-next lst)))
	(if (linklist-next lst)
	    (raise "Invalid linklist: inproper tail.")
	    (list))))

  (if (linklist-entry lst)
      (raise "Invalid linklist: no head.")
      (inner (linklist-next lst))))

(define (aforth-linklist ast)
  (cond
   [(list? ast)
    (list->linklist (for/list ([x ast]) (aforth-linklist x)))]

   ;; [(linklist? ast)
   ;;  (define (inner lst)
   ;;    (if (linklist-entry lst)
   ;;        (cons (aforth-linklist (linklist-entry lst)) 
   ;;      	(inner (linklist-next lst)))
   ;;        (if (linklist-next lst)
   ;;            (raise "Invalid linklist: inproper tail.")
   ;;            (list))))
    
   ;;  (if (linklist-entry ast)
   ;;      (raise "Invalid linklist: no head.")
   ;;      (inner (linklist-next ast)))
   ;;  ]

   [(forloop? ast)
    (define init (forloop-init ast))
    (forloop (if (list? init)
                 (list->linklist init)
                 (list->linklist (list init)))
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
              (aforth-linklist (funcdecl-body ast))
	      (funcdecl-simple ast))
    ]

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
  (define (constrain-mem l)
    (when (block? (linklist-entry l))
          (set-restrict-mem! (block-cnstr (linklist-entry l)) #t))
    (when (linklist-next l)
          (constrain-mem (linklist-next l))))
  
  ;; set head for from
  (define head (linklist #f #f from))
  (set-linklist-prev! from head)
  ;; set tail for to
  (set-linklist-next! to (linklist to #f #f))
  (constrain-mem from)
  
  #|(pretty-display "FROM!!!!!")
  (pretty-display (send to-string visit (linklist-entry from)))
  (pretty-display "TO!!!!!")
  (pretty-display (send to-string visit (linklist-entry to)))
  (pretty-display (send to-string visit from))|#
  
  ;; insert new funcdecl into program
  (define def-entry (first-funcdecl-linklist (aforth-code program)))
  (define pre-entry (linklist-prev def-entry))
  (define new-entry (linklist pre-entry (funcdecl new-name head #f) def-entry))
  (set-linklist-next! pre-entry new-entry)
  (set-linklist-prev! def-entry new-entry))

;; Mutate program by definig a new definition for the repeated structure. 
;; lst is a list of linklists that point to the starting of the structure
;; at multiple places in the program.
(define (extract-structure lst program)
  (when debug 
        (pretty-display `(extract-structure ,lst))
        (aforth-struct-print lst)
        )

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
      (let* ([bbb (linklist-entry x-linklist)]
             [cnstr (block-cnstr bbb)]
             [org (if (string? (block-org bbb)) 
                      (string-split (block-org bbb)) 
                      (block-org bbb))]
             [inout (estimate-inout x-code)]
             [a (estimate-a x-code)]
             [b (estimate-b x-code)]
             [r (estimate-r x-code)]
             )
        (set-block-body! bbb x-code)
        (set-block-in! bbb (car inout))
        (set-block-out! bbb (cdr inout))
        (set-block-cnstr! bbb (restrict (restrict-mem cnstr)
                                        (or (restrict-a cnstr) a) b r))
        (if (equal? location `front)
            (set-block-org! bbb (string-join (take org (length x-code))))
            (set-block-org! bbb (string-join (drop org (- (length org) (length x-code))))))
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
  (define prefix-cnstr #f)
  (define prefix-incnstr #f)
  ;; check that it is a linklist that contains block
  (when (and is-gap
             (andmap (lambda (x) (and (linklist? x) (block? (linklist-entry x)))) 
                     from-diffs))
      ;; if not off the list
      (let* ([entry (linklist-entry (car from-diffs))]
	     [org (block-org entry)]
	     [first-insts (if (string? org) (string-split org) org)]
             [revs (map (lambda (x)
                          (let* ([insts (block-body (linklist-entry x))]
                                 [insts-list (if (list? insts) insts (string-split insts))])
                            (reverse insts-list)))
                        from-diffs)]
             [pair (common-prefix revs)])
	(update from-diffs (map reverse (cdr pair)) `front)
	(set! prefix (reverse (car pair)))
        (set! prefix-org (drop first-insts (- (length first-insts) (length prefix))))
	(set! prefix-cnstr (block-cnstr entry))
	(set! prefix-incnstr (block-incnstr entry))
        ))
  
  (define suffix #f)
  (define suffix-org #f)
  (define suffix-cnstr #f)
  (define suffix-incnstr #f)
  ;; check that it is a linklist that contains block
  (when (and is-gap
             (andmap (lambda (x) (and (linklist? x) (block? (linklist-entry x)))) 
                     to-diffs))
      ;; if not off the list
      (let* ([entry (linklist-entry (car to-diffs))]
	     [org (block-org entry)]
             [first-insts (if (string? org) (string-split org) org)]
             [forwards (map (lambda (x)
			     (let* ([insts (block-body (linklist-entry x))]
                                    [insts-list (if (list? insts) insts (string-split insts))])
                               insts-list))
                            to-diffs)]
             [pair (common-prefix forwards)])
        (update to-diffs (cdr pair) `back)
	;; TODO suffix-org
	(set! suffix (car pair))
        (set! suffix-org (take first-insts (length suffix)))
	(set! suffix-cnstr (block-cnstr entry))
	(set! suffix-incnstr (block-incnstr entry))
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
    (let ([inout (estimate-inout prefix)]
	  [a (estimate-a prefix)]
	  [b (estimate-b prefix)]
	  [r (estimate-r prefix)]
          )
      (set! from (linklist #f 
                           (new-block prefix (car inout) (cdr inout) 
                                      (restrict (restrict-mem prefix-cnstr) 
                                                (or (restrict-a prefix-cnstr) a) 
                                                b r)
                                      prefix-incnstr
                                      prefix-org)
                           from))))
  
  (when suffix
    (let* ([inout (estimate-inout suffix)]
	   [a (estimate-a suffix)]
	   [b (estimate-b suffix)]
	   [r (estimate-r suffix)]
           [suffix-linklist (linklist to 
                                      (new-block suffix (car inout) (cdr inout)
                                                 (restrict (restrict-mem suffix-cnstr) 
                                                           (or (restrict-a suffix-cnstr) a) 
                                                           b r)
                                                 suffix-incnstr
                                                 suffix-org)
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

(define (extract-all-sequence linklist-program min-len occur)
  
  (define (get-linklist ll index)
    (define entry (linklist-entry ll))
    (when debug
          (if (block? entry)
              (pretty-display `(get-linklist ,(block-body entry) ,index))
              (pretty-display `(get-linklist ,(funccall-name entry) ,index))))
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
	(if (equal? insts "")
	    (get-linklist (linklist-next ll) index)
	    (get-linklist (linklist-next ll) (- index (string-length insts) 1))))) ;; minus 1 for space
    
  ;; Split linklist ll into 2 linklists by inserting a new linklist
  ;; after the splitted one.
  (define (split-linklist ll index code org)
    (when debug 
          (pretty-display `(split-linklist ,(substring code 0 index) ,(substring code index)))
          (pretty-display "ll:")
          (aforth-struct-print (linklist-entry ll)))
    ;; (pretty-display `(org ,org))
    (let* ([n (length (string-split (substring code 0 index)))]
           [code-list (string-split code)]
           [org-list (string-split org)]

           [insts (string-join (take code-list n))]
           [fst-org (string-join (take org-list n))]
           [inout (estimate-inout insts)]
           [a     (estimate-a insts)]
           [b     (estimate-b insts)]
           [r     (estimate-r insts)]
           [entry (linklist-entry ll)]
	   [mem   (restrict-mem (block-cnstr entry))]
	   [org-a (restrict-a (block-cnstr entry))]
	   [org-r (restrict-r (block-cnstr entry))]

           [snd-insts (string-join (drop code-list n))]
           [snd-org   (string-join (drop org-list n))]
           [snd-inout (estimate-inout snd-insts)]
           [snd-a     (estimate-a snd-insts)]
           [snd-b     (estimate-b snd-insts)]
           [snd-r     (estimate-r snd-insts)]
           [snd-in    (block-incnstr entry)]
           [new-linklist (linklist ll
                                   (if snd-org
                                       (new-block snd-insts (car snd-inout) (cdr snd-inout)
					      ;; need to handle a special because of @+/!+ opt
                                              (restrict mem (or org-a snd-a) snd-b snd-r)
                                              snd-in
                                              snd-org)
                                       (funccall snd-insts))
                                   (linklist-next ll))])
      (set-block-body! entry insts)
      (set-block-org! entry fst-org)
      (set-block-in! entry (car inout))
      (set-block-out! entry (cdr inout))
      ;; need to handle a special because of @+/!+ opt
      (set-block-cnstr! entry (restrict mem (or org-a a) b r))
      (set-linklist-prev! (linklist-next ll) new-linklist)
      (set-linklist-next! ll new-linklist)))
  
  (define (replace-with head func exp)
    (define ll (linklist-next head))
    (define pair (first-location exp ll))
    (define from (car pair))
    (define to (cdr pair))
    (define len (- to from))

    (define-values (ll-from index-from code-from org-from) (get-linklist ll from))

    (when (> index-from 0)
      (when debug (pretty-display "(> index-from 0)"))
      (split-linklist ll-from index-from code-from org-from)
      (set! ll-from (linklist-next ll-from))
      )

    (define-values (ll-to index-to code-to org-to) (get-linklist ll-from (sub1 len)))
    (when debug
	  (pretty-display "----------------------")
	  (pretty-display "ll-from:")
	  (aforth-struct-print (linklist-entry ll-from))
	  (pretty-display `(len ,len))
	  (pretty-display "ll-to:")
	  (aforth-struct-print (linklist-entry ll-to))
	  (pretty-display `(,index-to ,code-to ,org-to)))
    (set! index-to (add1 index-to))

    (when (< index-to (string-length code-to))
      (when debug (pretty-display "(< index-to)"))
      (split-linklist ll-to index-to code-to org-to)
      (when debug (pretty-display "(< index-to) done"))
      )
  
    (define from-prev (linklist-prev ll-from))
    (define to-next (linklist-next ll-to))
    (when debug 
	  (pretty-display "from-prev:")
	  (aforth-struct-print (linklist-entry from-prev))
	  (pretty-display "ll-from:")
	  (aforth-struct-print (linklist-entry ll-from))
	  (pretty-display "ll-to:")
	  (aforth-struct-print (linklist-entry ll-to))
	  (pretty-display "to-next:")
	  (aforth-struct-print (linklist-entry to-next)))
    
    (define new-linklist (linklist from-prev func to-next))
    (set-linklist-next! from-prev new-linklist)
    (set-linklist-prev! to-next new-linklist)
    
    (cons ll-from ll-to)
    )

  (define (define-and-replace locations exp)
    (define new-name (new-def))
    (when debug (pretty-display locations))
    (define res-list
      (for/list ([location locations])
		(replace-with location (funccall new-name) exp)))

    (define res (car res-list))

    (define (combine-restriction lst)
      ;; combine restriction in the first one.
      (define first-item (car lst))
      (when (block? (linklist-entry first-item))
	    (define first-block (linklist-entry first-item))
	    (define a #f)
	    (define b #f)
	    (define mem #f)
	    (define r #f)
	    (for ([item (map linklist-entry lst)])
		 (set! a (or a (restrict-a (block-cnstr item))))
		 (set! b (or b (restrict-b (block-cnstr item))))
		 (set! r (or r (restrict-r (block-cnstr item))))
		 (set! mem (or mem (restrict-mem (block-cnstr item)))))
	    (set-restrict-a! (block-cnstr first-block) a)
	    (set-restrict-b! (block-cnstr first-block) b)
	    (set-restrict-r! (block-cnstr first-block) r)
	    (set-restrict-mem! (block-cnstr first-block) mem))

      (unless (equal? first-item (cdr res))
	      (combine-restriction (map linklist-next lst))))

    (combine-restriction (map car res-list))

    (when (block? (linklist-entry (car res)))
      ;; If initial state constraints of the repeating seqeunces are different,
      ;; need to set it to false.
      (let* ([def-block (linklist-entry (car res))]
	     [incnstr (block-incnstr def-block)]
	     [same
	      (for/and ([x (cdr res-list)])
		       (equal? (block-incnstr (linklist-entry (car x))) incnstr))])
	(unless same
		(set-block-incnstr! def-block #f))))

    (insert-definition (car res) (cdr res) linklist-program new-name))
  
  (define-values (seqs max-len) (send (new sequence-miner%) visit linklist-program))
  (define subseqs (sort-subsequence seqs min-len max-len 
                                    (add1 (quotient (* 4 occur) (sub1 occur)))))
  
  (when debug (pretty-display subseqs))
  
  (for ([subseq subseqs])
    (let ([str (string-join subseq)])
      ;; Can't define repeating sequence that contains "push" or "pop" becuase
      ;; it will mess up the return stack.
      (unless (or (regexp-match #rx"push" str) (regexp-match #rx"pop" str))
        (let* ([reformatted (string-replace (string-replace str "+" "\\+") "*" "\\*")]
               [exp (regexp reformatted)]
               [matcher (new sequence-matcher% [exp exp])]
               [locations (send matcher visit linklist-program)])
          (when debug
                (pretty-display (format "SEQ: ~a  OCCUR: ~a" str (length locations))))
          (when (>= (length locations) occur)
                ;; if repeat more then a certain number
                 (when debug (pretty-display (format "STRING: ~a" reformatted)))
                 (define-and-replace locations exp)
                                        ;(aforth-syntax-print linklist-program 2 2)
                 )
          ))))
  
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
    (when debug (pretty-display `(topo-sort ,lst)))
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
          (cons func (topo-sort updated)))))

  (define start (first-funcdecl-linklist (aforth-code program)))
  (define funcdecls (topo-sort (get-funcdecl start)))
  (define new-start (list->linklist funcdecls))
  (set-linklist-next! (linklist-prev start) (linklist-next new-start))
  (set-linklist-prev! (linklist-next new-start) (linklist-prev start))
)

(define (define-repeating-code program)
  (if (and program (aforth-code program))
      (let ([linklist-program (aforth-linklist program)])
        (when debug (pretty-display ">>> EXTRACT-STRUCTURES"))
	(extract-all-structure linklist-program)
        (when debug (pretty-display ">>> EXTRACT-SEQUENCES"))
	(extract-all-sequence linklist-program 6 2)

        ;; TODO: if the code doesn't fit in, do this
        ;(when debug (pretty-display ">>> EXTRACT-SEQUENCES"))
	;(extract-all-sequence linklist-program 3 4)

	(reorder-definition linklist-program)
        ;(send (new block-merger%) visit linklist-program)
        linklist-program
	)
      program)
)

(define (define-repeating-codes programs w h)
  (define res (make-vector (* w h)))
  (for ([i (in-range (* w h))])
       (pretty-display (format "--------------- define-repeating ~a ------------------" i))
       (vector-set! res i (define-repeating-code (vector-ref programs i))))
  res)

