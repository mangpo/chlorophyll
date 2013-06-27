#lang s-exp rosette

(require "../layout.rkt" "../layout-map.rkt")

(provide (all-defined-out))

(define (parse-input file)
  (with-input-from-file file
    (lambda ()
      (let* ((params (map string->number (string-split (read-line))))
	     (n (car params))
	     (width (cadr params))
	     (height (caddr params))
	     (layout-map (make-layout-map)))
	(for ([i (range n)]
	      [core (make-symbolic-physical-cores n width height)])
	     ; Use the max size to enforce separate cores
	     ; Initially no dependencies
	     (add-layout-entry! layout-map i 256 core '()))

	(define (loop)
	  (let ((line (map string->number (string-split (read-line)))))
	    (unless (null? line)
		    (let ((id1 (car line))
			  (id2 (cadr line))
			  (num-msg (caddr line)))
		      (let ((val1 (id->val layout-map id1))
			    (val2 (id->val layout-map id2)))
			(add-layout-entry! layout-map id1 256
					   (val->core val1)
					   (cons (cons (val->core val2)
						       num-msg)
						 (val->dependencies val1)))
			(add-layout-entry! layout-map id2 256
					   (val->core val2)
					   (cons (cons (val->core val1)
						       num-msg)
						 (val->dependencies val2)))))
		    (loop))))
	(loop)
	layout-map))))

(define (test-file file)
  (solve-layout (parse-input file) #:max-cost 100000))

(test-file "md5.in")
