#lang s-exp rosette

(require "header.rkt"
         "ast.rkt" "parser.rkt" "visitor-interface.rkt" "space-estimator.rkt" 
         "symbolic-dict.rkt")

(provide (all-defined-out))

(define debug #f)

(define count-msg-interpreter%
  (class* object% (visitor<%>)
    (super-new)
    (init-field core-space num-core [env (make-hash)] [places (make-cores #:capacity core-space #:max-cores num-core)])

    ;;; Increase the used space of "place" by "add-space".
    (define (inc-space place add-space)
      (cores-inc-space places place add-space))
    
    (define (inc-space-with-op place op)
      ;(cores-add-op places place op)
      (cores-inc-space places place (est-space op))
      )

    ;;; Count number of message passes. If there is a message pass, it also take up more space.
    (define (count-msg x-ast y-ast)
      
      ;; Add comm space to cores
      (define (add-comm x)
        (if (number? x)
             (inc-space x est-comm)
             (for ([p (car x)])
                  (inc-space (get-field place p) est-comm))))

      ;; Hash value of (list of RangePlace% object) and index
      ;; (define (hash-val pair)
      ;;   (let* ([places (car pair)]
      ;;          [index  (cdr index)])
      ;;     (+ (* (foldl (lambda (p res) (+ (* res 37) (send p hash-code))) 17 places) 37)
      ;;        (send index hash-code))))

      (define (hash-val pair)
        (equal-hash-code pair))

      ;; Return the place that a resides if a only lives in one place. 
      ;; Otherwise, return hash of a.
      (define (get-place a)
	(if (number? a)
	    a
	    (if (= (length (car a)) 1)
		(get-field place (car a))
		(hash-val a))))

      (define (same-place? a b)
	(equal? (get-place a) (get-place b)))

      (define (count-comm p)
	(if (number? p)
	    1
	    (if (get-field known-type (cdr p))
		1
		(length (car p)))))
	    
      (define x (get-field place x-ast))
      (define y (get-field place y-ast))
      (define x-comm (count-comm x))
      (define y-comm (count-comm y))

      (cond 
        ;[(equal? x 0) 0] ;any
        ;[(equal? y 0) 0] ;any

        ;; if x and y are the same place.
        [(same-place? x y) 0]

	;; if x and y are in a single core.
	[(and (= x-comm 1) (= y-comm 1))
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display "COMM + 1"))
	 1]

        ;; if x or y is in multiple cores.
        [else 
         (add-comm x)
         (add-comm y)
         (when debug (pretty-display (format "COMM + ~a + ~a" x-comm y-comm)))
         (+ x-comm y-comm)])) ;; TODO: not 1 if it's not an array of statically known

    (define (lookup ast)
      (dict-ref env (get-field name ast) (lambda () (send ast not-found-error))))
    
    (define (place-at places index ast)
      (when (empty? places)
            (send ast index-out-of-bound index))
      (let* ([current (car places)]
             [from    (get-field from current)]
             [to      (get-field to current)])
        (if (and (>= index from) (< index to))
            (get-field place current)
            (place-at (cdr places) index ast))))
        
    
    (define/public (assert-capacity)
      (cores-assert places))

    (define/public (display-used-space)
      (display-cores places))

    (define/public (num-cores)
      (cores-count places))
      
    (define/public (visit ast)
      (cond
       [(is-a? ast Num%)
          (when debug (pretty-display (format "Num ~a" (get-field n ast))))
          (inc-space (get-field place ast) est-num)
          0]
       
       [(is-a? ast Array%)
          ;; lookup place from env
          (define place-known (lookup ast))
          (define places (car place-known))
          (define known (cdr place-known))
          (set-field! known-type ast known)

	  (define index (get-field index ast))
	  (define index-count (send index accept this))

          (when debug (pretty-display (format "Array ~a" (get-field name ast))))

          (if (= (length places) 1)
              ;; Array lives in only one place
              (set-field! place ast (get-field place (car places)))
	      (if (is-a? index Num%)
		  ;; Know exact index
		  (set-field! place ast (place-at places (get-field n index) ast))
		  ;; Extract list of possible places
		  (set-field! place ast (cons places index))))
          
          (+ index-count (count-msg index ast))]

       [(is-a? ast Var%) ; multiple places?
          ;; lookup place from env
          (define place-known (lookup ast))
          (send ast set-place-known place-known)

          (when debug (pretty-display (format "Var ~a" (get-field name ast))))
          (inc-space (get-field place ast) est-var)
          0]


       [(is-a? ast UnaExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e1-count (send e1 accept this))
          (define op-place (get-field place ast))
          
          ;; set known type
          (set-field! known-type ast (get-field known-type e1))

          (when debug
                (pretty-display (format "UnaOp ~a" (get-field op (get-field op ast)))))
          (inc-space-with-op op-place (get-field op (get-field op ast))) ; increase space
          
          (+ e1-count (count-msg ast e1))]

       [(is-a? ast BinExp%)
          (when debug (newline))
          (define e1 (get-field e1 ast))
          (define e2 (get-field e2 ast))
          (define e1-count (send e1 accept this))
          (define e2-count (send e2 accept this))
          (define op-place (get-field place ast))
          
          ;; set known type
          (set-field! known-type ast (and (get-field known-type e1) (get-field known-type e2)))

          (when debug
                (pretty-display (format "BinOp ~a" (get-field op (get-field op ast)))))
          (inc-space-with-op op-place (get-field op (get-field op ast))) ; increase space

          (+ (+ (+ e1-count e2-count)
                      (count-msg ast e1))
                   (count-msg ast e2))]
                
       [(is-a? ast VarDecl%) 
          (define place-known (send ast get-place-known))
          (define place (get-field place ast))
          (define var-list (get-field var-list ast))
          
          ;; put vars into env
          (for ([var var-list])
               (dict-set! env var place-known))

          (when debug
                (pretty-display (format "VarDecl ~a" var-list)))

	  ;; increase space for variable
          (inc-space place (* (length var-list) est-data)) ; include space

          0]

       [(is-a? ast ArrayDecl%)
          (define place-known (send ast get-place-known))

          ;; check boundaries
          (define last 0)

          (for ([p (car place-known)])
	       (let* ([from (get-field from p)]
		      [to   (get-field to p)])
		 (when (not (= from last))
		       (send ast bound-error))
		 (set! last to)
		 (inc-space (get-field place p) (* (- to from) est-data))
	       ))

          (when (not (= (get-field bound ast) last))
                (send ast bound-error))

          ;; put array into env
          (dict-set! env (get-field var ast) place-known)

          0]

       [(is-a? ast Assign%) 
          (when debug (newline))
          (define lhs (get-field lhs ast))
          (define rhs (get-field rhs ast))

          (when debug
                (pretty-display "Assign"))
          ;;; Visit lhs
          (send lhs accept this)

          (define lhs-place (get-field place lhs))
          (define lhs-known (get-field known-type lhs))

          ;;; If rhs is a number, set place to be equal to lhs
          (when (is-a? rhs Num%) (set-field! place rhs lhs-place))

          ;;; Visit rhs
          (define rhs-count (send rhs accept this))

          ;;; Update dynamic known type
          (define rhs-known (get-field place rhs))
          (when (and (not rhs-known) lhs-known)
                (set-field! known-type lhs #f)
                (dict-set! env (get-field name lhs) (send lhs get-place-known)))
       
          (+ rhs-count (count-msg lhs rhs))
        ]

       [(is-a? ast Block%) 
          (foldl (lambda (stmt sum) (+ sum (send stmt accept this))) 
                 0 
                 (get-field stmts ast))]
       [else (raise "Error: count-msg-interpreter unimplemented!")]))
))

