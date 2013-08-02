#lang racket

(require "header.rkt" "arrayforth.rkt" "arrayforth-print.rkt")

(provide structure-extractor% sequence-miner%)

(define to-string (new string-converter%))

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

    (define/public (visit ast)
      (cond
       [(linklist? ast)
        (define entry (linklist-entry ast))
        (define insts (list))

        (define (collect-from-block x)
          (define entry (linklist-entry x))
          (cond
           [(block? entry)
            (let* ([body (block-body entry)]
                   [seq (if (string? body) (string-split body) body)])
              (set! insts (append insts seq))
              (collect-from-block (linklist-next x)))]

           [(funccall? entry)
            (set! insts (append insts (list (funccall-name entry))))
            (collect-from-block (linklist-next x))]

           [else x]))

        (define next (collect-from-block ast))
        (set! seqs (set-add seqs insts))
        (send this visit (linklist-entry next))
        (send this visit (linklist-next next))]

       [(block? ast)
        (define body (block-body ast))
        (set! seqs (set-add seqs (if (string? body) (string-split body) body)))]

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
	]
       
       [else
        (rasie (format "sequence-miner: unimplemented for ~a" ast))]))))