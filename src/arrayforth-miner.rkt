#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt")

(provide (all-defined-out))

(define to-string (new string-converter%))


(define (collect-from-block x [insts (list)])
  (define entry (linklist-entry x))
  (cond
   [(block? entry)
    (define body (block-body entry))
    (define seq (if (string? body) (string-split body) body))
    (set! insts (append insts seq))
    (collect-from-block (linklist-next x) insts)]
   
   [(funccall? entry)
    (set! insts (append insts (list (funccall-name entry))))
    (collect-from-block (linklist-next x) insts)]
   
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

       [(funcdecl? ast)
	(send this visit (funcdecl-body ast))]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	(findf (lambda (lst) (> (length lst) 1))
	       (hash-values structure))
	]
       
       [(list? ast)
        (raise "structure-extractor: please convert aforth structure to linklist first.")]
       
       [else void]))))

(define sequence-miner%
  (class object%
    (super-new)
    (define seqs (set))
    (define lens (list))

    (define/public (visit ast)
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast))
 	;; (define next (car ret))		
	;; (define insts (cdr ret))
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
	(send this visit (funcdecl-body ast))]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	;; max length
	(define max-len (foldl (lambda (x res) (max x res)) 0 lens))
	;; second max length
	(define snd-len (foldl (lambda (x res) (max x res)) 0 (remove max-len lens)))

	(values seqs snd-len)
	]
       
       [else void]))))


(define sequence-matcher%
  (class object%
    (super-new)
    (init-field exp)
    (define result (list)) ;; list of (linklist . start-pos)

    (define/public (visit ast)
      (define (add-to-result insts)
	(define matches (regexp-match-positions* exp (string-join insts)))
	
	(for ([pos matches])
	     (set! result (cons (cons ast pos) result))))
	
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast))
        (add-to-result insts)

        (send this visit (linklist-entry next))
        (send this visit (linklist-next next))]

       [(block? ast)
        (define body (block-body ast))
	(define insts (if (string? body) (string-split body) body))
        (add-to-result insts)
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
	(send this visit (funcdecl-body ast))]

       [(aforth? ast)
	(send this visit (aforth-code ast))
	result
	]
       
       [else void]))))

;; Arg: set of sequences of instructions, 
;;      minimum length of output sequence of insturction
;; Return list of sequences of instructions
;; - that are subsequences of the given sequences
;; - whose length is >= min-len
;; - sorted by length
(define (sort-subsequence seq-set min-len max-len)
  (define filtered (filter (lambda (x) (>= (length x) min-len)) (set->list seq-set)))
  (define result (list))

  (for* ([len (in-range (add1 max-len) min-len -1)]
	 [seq filtered]
	 [i (in-range (add1 (- (length seq) len)))])
	(set! result (cons (take (drop seq i) len) result)))

  (reverse result))
