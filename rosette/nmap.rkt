#lang racket

; An n-map provides a one-to-one-to-one-to... mapping.  Each nmap
; contains ntuples.  ntuples can be accessed by any of their
; elements.  Since we want a one-to-one-to... mapping, when we add a
; new tuple, any existing bindings for any of the keys are removed.

(provide (all-defined-out))

; An n-map is a list of n hashmaps.
; The nth hashmap maps the nth key to the k-tuple value.
; Assumption: n >= 1
(define (make-nmap n)
  (build-list n (lambda (x) (make-hash))))

(define (add-ntuple! nmap . args)
  (unless (<= (length nmap) (length args))
	  (error "Incorrect number of arguments to add-ntuple"))
  (let* ((n (length nmap))
	 (keys (take args n)))
    (for-each (lambda (index key) (remove-by-nth-key! nmap index key))
	      (range n) keys)
    (for-each (lambda (hash key) (hash-set! hash key args))
	      nmap keys)))

(define (get-by-nth-key nmap index key)
  (hash-ref (list-ref nmap index) key))

(define (get-all-values nmap)
  (hash-values (car nmap)))

; Gets all of the values where the item at position index is val.
; Note that if there are m keys and n elements in the value, then the
; index should satisfy m <= index < n.
; For index < m, should just used get-by-nth-key.
(define (get-list-by-nth-val nmap index val)
  (filter (lambda (x) (equal? (list-ref x index) val))
	  (get-all-values nmap)))

(define (has-nth-key? nmap index key)
  (hash-has-key? (list-ref nmap index) key))

; Removes the mapping, if it exists.
; If it does not exist, does nothing.
(define (remove-by-nth-key! nmap index key)
  (when (has-nth-key? nmap index key)
	(for-each hash-remove! nmap
		  (take (get-by-nth-key nmap index key) (length nmap)))))
