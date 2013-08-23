#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt")

(provide (except-out (all-defined-out) to-string))

(define to-string (new string-converter%))


(define (collect-from-block x [func #f] [insts (list)])
  (define entry (linklist-entry x))
  (cond
   [(block? entry)
    (define body (block-body entry))
    (define seq (if (string? body) (string-split body) body))
    (set! insts (append insts seq))
    (collect-from-block (linklist-next x) func insts)]
   
   [(funccall? entry)
    (if (and func (equal? func (funccall-name entry)))
	(values x insts)
	(begin
	  (set! insts (append insts (list (funccall-name entry))))
	  (collect-from-block (linklist-next x) func insts)))]
   
   [else (values x insts)]))

(define structure-extractor%
  (class object%
    (super-new)
    (define structure (make-hash))
    (define parent #f)

    
    (define/public (visit ast)
      (define (hash-structure)
	(define str (send to-string visit ast))
	(if (hash-has-key? structure str)
	    (hash-set! structure str (cons parent (hash-ref structure str)))
	    (hash-set! structure str (list parent))))

      (cond
       [(linklist? ast)
	(set! parent ast)
	(send this visit (linklist-entry ast))
	(when (linklist-next ast)
	      (send this visit (linklist-next ast)))]

       [(forloop? ast)
	(hash-structure)
        (send this visit (forloop-body ast))]

       [(ift? ast)
	(hash-structure)
	(send this visit (ift-t ast))]

       [(iftf? ast)
	(hash-structure)
	(send this visit (iftf-t ast))
	(send this visit (iftf-f ast))]

       [(-ift? ast)
	(hash-structure)
	(send this visit (-ift-t ast))]

       [(-iftf? ast)
	(hash-structure)
	(send this visit (-iftf-t ast))
	(send this visit (-iftf-f ast))]

       [(mult? ast)
        (hash-structure)]

       [(funcdecl? ast)
	(send this visit (funcdecl-body ast))]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	;; return a list of linklists of the first repeated structure found.
	(findf (lambda (lst) (> (length lst) 1))
	       (hash-values structure))

	;; (pretty-display ">>> ALL-PAIRS")
	;; (pretty-display (hash->list structure))
	;; (define filtered-pairs (filter (lambda (pair) (> (length (cdr pair)) 1))
	;; 			       (hash->list structure)))
	;; (pretty-display ">>> FILTERED-PAIRS")
	;; (pretty-display filtered-pairs)
	;; (pretty-display ">>>")
	;; (if (empty? filtered-pairs)
	;;     #f
	;;     (cdr (argmax (lambda (pair) (string-length (car pair))) filtered-pairs)))
	]
       
       [(list? ast)
        (raise "structure-extractor: please convert aforth structure to linklist first.")]
       
       [else void]))))

(define sequence-miner%
  (class object%
    (super-new)
    (define seqs (set))
    (define lens (list))
    (define func #f)

    (define/public (visit ast)
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast func))
	;(pretty-display (format "FUNC: ~a, SEQ: ~a" func insts))
        (set! seqs (set-add seqs insts))
	(set! lens (cons (length insts) lens))
        (send this visit (linklist-entry next))
        (send this visit (linklist-next next))]

       [(block? ast)
        (define body (block-body ast))
	(define insts (if (string? body) (string-split body) body))
        (set! seqs (set-add seqs insts))
	(set! lens (cons (length insts) lens))
	]

       [(forloop? ast)
        (send this visit (forloop-init ast))
        (send this visit (forloop-body ast))]

       [(ift? ast)
	(send this visit (ift-t ast))]

       [(iftf? ast)
	(send this visit (iftf-t ast))
	(send this visit (iftf-f ast))]

       [(-ift? ast)
	(send this visit (-ift-t ast))]

       [(-iftf? ast)
	(send this visit (-iftf-t ast))
	(send this visit (-iftf-f ast))]

       [(funcdecl? ast)
	(set! func (funcdecl-name ast))
	(send this visit (funcdecl-body ast))
	(set! func #f)
	]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	;; max length
	(define max-len (foldl (lambda (x res) (max x res)) 0 
                               lens))
	;; second max length
	(define snd-len (foldl (lambda (x res) (max x res)) 0 
                               (remove max-len lens)))

	(values seqs snd-len)
	]
       
       [else void]))))

(define (first-location exp ast)
  (define (contain-block-funccall x)
    (if (or (block? (linklist-entry x)) (funccall? (linklist-entry x)))
        x 
        (contain-block-funccall (linklist-next x))))
  
  (define-values (next insts) 
    (collect-from-block (contain-block-funccall ast)))
  (car (regexp-match-positions exp (string-join insts))))

(define sequence-matcher%
  (class object%
    (super-new)
    (init-field exp)
    (define result (list)) ;; list of (linklist . start-pos)
    (define func #f)

    (define/public (visit ast)
      (define (add-to-result insts)
	(define matches (regexp-match-positions* exp (string-join insts)))
	
        ;; add prev pointers to result list
	(for ([pos matches])
             ;; (pretty-display "ADD TO RESULT:")
             ;; (aforth-struct-print (linklist-prev ast))
	     (set! result (cons (linklist-prev ast) result))))
	
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast func))
        (add-to-result insts)

        (send this visit (linklist-entry next))
        (send this visit (linklist-next next))]

       [(forloop? ast)
        (send this visit (forloop-init ast))
        (send this visit (forloop-body ast))]

       [(ift? ast)
	(send this visit (ift-t ast))]

       [(iftf? ast)
	(send this visit (iftf-t ast))
	(send this visit (iftf-f ast))]

       [(-ift? ast)
	(send this visit (-ift-t ast))]

       [(-iftf? ast)
	(send this visit (-iftf-t ast))
	(send this visit (-iftf-f ast))]

       [(funcdecl? ast)
	(set! func (funcdecl-name ast))
	(send this visit (funcdecl-body ast))
	(set! func #f)
	]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	result
	]
       
       [else void]))))

(define dependencey-collector%
  (class object%
    (super-new)
    (define result (set))
    (define func #f)

    (define/public (visit ast)
	
      (cond
       [(linklist? ast)
        (send this visit (linklist-entry ast))
        (send this visit (linklist-next ast))]

       [(forloop? ast)
        (send this visit (forloop-init ast))
        (send this visit (forloop-body ast))]

       [(ift? ast)
	(send this visit (ift-t ast))]

       [(iftf? ast)
	(send this visit (iftf-t ast))
	(send this visit (iftf-f ast))]

       [(-ift? ast)
	(send this visit (-ift-t ast))]

       [(-iftf? ast)
	(send this visit (-iftf-t ast))
	(send this visit (-iftf-f ast))]

       [(funcdecl? ast)
	(set! result (set))
	(set! func (funcdecl-name ast))
	(send this visit (funcdecl-body ast))
	(set! func #f)
	result
	]

       [(funccall? ast)
	(unless (equal? func (funccall-name ast))
		(set! result (set-add result (funccall-name ast))))]
       
       [else void]))))

(define block-merger%
  (class object%
    (super-new)

    (define/public (visit ast)
	
      (cond
       [(linklist? ast)
        (when (and (linklist-entry ast)
                   (block? (linklist-entry ast))
                   (linklist-entry (linklist-next ast))
                   (block? (linklist-entry (linklist-next ast))))
              (let* ([a-block (linklist-entry ast)]
                     [b-linklist (linklist-next ast)]
                     [b-block (linklist-entry b-linklist)]
                     [a-body (block-body a-block)]
                     [b-body (block-body b-block)]
                     [a-list (if (string? a-body) 
                                 (string-split a-body) a-body)]
                     [b-list (if (string? b-body)
                                 (string-split b-body) b-body)])
                (when (<= (+ (length a-list) (length b-list)) block-limit)
                      (let ([next (linklist-next b-linklist)])
                        (merge-block a-block b-block)
                        (set-linklist-next! ast next)
                        (set-linklist-prev! next ast)))))

        (send this visit (linklist-entry ast))
        (send this visit (linklist-next ast))]

       [(forloop? ast)
        (send this visit (forloop-init ast))
        (send this visit (forloop-body ast))]

       [(ift? ast)
	(send this visit (ift-t ast))]

       [(iftf? ast)
	(send this visit (iftf-t ast))
	(send this visit (iftf-f ast))]

       [(-ift? ast)
	(send this visit (-ift-t ast))]

       [(-iftf? ast)
	(send this visit (-iftf-t ast))
	(send this visit (-iftf-f ast))]

       [(funcdecl? ast)
	(send this visit (funcdecl-body ast))
	]
       
       [(aforth? ast)
	(send this visit (aforth-code ast))]

       [else void]))))


;; Arg: set of sequences of instructions, 
;;      minimum length of output sequence of insturction
;; Return list of sequences of instructions
;; - that are subsequences of the given sequences
;; - whose length is >= min-len
;; - sorted by length
(define (sort-subsequence seq-set min-len max-len min-actual-len)

  (define (program-length program)
    (define (occupy x)
      (cond
       [(member x (list "@+" "@b" "@" "!p" "!+" "!b" "!" "+*" "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop" "over" "a" "nop" "." "push" "b!" "a!"))
        1]
       [(or (string->number x) (member x (list "up" "down" "left" "right" "io")))
        5]
       [else 4]))

    (foldl (lambda (x sum) (+ sum (occupy x))) 0 program))
    
  (define filtered (filter (lambda (x) (>= (length x) min-len)) (set->list seq-set)))
  (define result (list))

  (for* ([len (in-range max-len (sub1 min-len) -1)]
	 [seq filtered]
	 [i (in-range (add1 (- (length seq) len)))])
	(set! result (cons (take (drop seq i) len) result)))

  (reverse (filter (lambda (x) (>= (program-length x) min-actual-len)) result)))
