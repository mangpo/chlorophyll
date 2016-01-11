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
    (collect-from-block (linklist-next x) func 
			(append insts seq))]
   
   [(funccall? entry)
    (if (and func (equal? func (funccall-name entry)))
	(values x insts)
	(collect-from-block (linklist-next x) func 
			    (append insts (list (funccall-name entry)))))]
   
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

       [(or (mult? ast) (abs? ast) (port-exec? ast) (port-listen? ast))
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
    (define collect #f)

    (define/public (visit ast)
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast func))
	;(pretty-display (format "FUNC: ~a, SEQ: ~a" func insts))
	(when collect
	      (set! seqs (set-add seqs insts))
	      (set! lens (cons (length insts) lens)))
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
	(set! collect (not (funcdecl-simple ast)))

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

  ;; (pretty-display `(first-location ,(string-join insts) ,exp))
  ;; (pretty-display (car (regexp-match-positions exp (string-join insts))))
  (car (regexp-match-positions exp (string-join insts))))

(define sequence-matcher%
  (class object%
    (super-new)
    (init-field exp)
    (define result (list)) ;; list of (linklist . start-pos)
    (define func #f)
    (define collect #t)

    (define/public (visit ast)
      (define (add-to-result insts)
        (define str  (string-join insts))
        (define len (string-length str))
	(define matches (filter (lambda (x) 
				  (let ([start (car x)]
					[end (cdr x)])
				    (and (or (= start 0)
					     (equal? (substring str (sub1 start) start) " "))
					 (or (= end (string-length str))
					     (equal? (substring str end (add1 end)) " ")))))
				(regexp-match-positions* exp str)))
	;; (pretty-display `(add-to-result ,insts ,matches))
	
        ;; add prev pointers to result list
	(for ([pos matches])
             ;; (pretty-display (format "ADD TO RESULT: ~a" exp))
             ;; (aforth-struct-print (linklist-prev ast))
             (let ([to (cdr pos)])
               (when (or (= (cdr pos) len) 
                         (equal? (substring str to (add1 to)) " "))
                     (set! result (cons (linklist-prev ast) result))))))
	
      (cond
       [(linklist? ast)
        (define-values (next insts) (collect-from-block ast func))
	(when collect
	      (add-to-result insts))

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
	(set! collect (not (funcdecl-simple ast)))
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
    (init-field w [dependent-nodes (set)])
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

       [(port-exec? ast)
        (set! dependent-nodes (set-add dependent-nodes
                                       (core-id (port-exec-at ast) w)
                                       ))
        ]

       [(funcdecl? ast)
	(set! result (set))
	(set! func (funcdecl-name ast))
	(send this visit (funcdecl-body ast))
	(set! func #f)
	result
	]

       [(funccall? ast)
        (define name (funccall-name ast))
	(unless (member name (list func "in" "out" "--u/mod" "*.17" "*."))
		(set! result (set-add result (funccall-name ast))))]
       
       [else void]))))

(define block-merger%
  (class object%
    (super-new)

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
