#lang s-exp rosette

(require "header.rkt"
         "space-estimator.rkt")

(provide (all-defined-out))

(struct core (space costly-op))

(define capacity 256)
(define max-cores 144)

(define (display-cores cores)
  (let* ([space (core-space cores)]
         [costly-op (core-costly-op cores)])
        (for ([i (in-range 0 max-cores)])
          (pretty-display (format "core = ~a, space = ~a, ops = ~a" 
                                  i (evaluate-with-sol (vector-ref space i)) 
                                  (evaluate-with-sol (vector-ref costly-op i)))))))

(define (cores-evaluate cores)
  (let* ([space (core-space cores)]
         [costly-op (core-costly-op cores)])
    (for ([i (in-range 0 max-cores)])
         (vector-set! space i (evaluate-with-sol (vector-ref space i)))
         (vector-set! costly-op i (evaluate-with-sol (vector-ref costly-op i))))))

;; Check if two set of cores are the same in term of used space and costly operations.
(define (cores-equal? c1 c2)
  (and (equal? (core-space c1) (core-space c2))
       (equal? (core-costly-op c1) (core-costly-op c2))))
  
(define (make-cores #:capacity [c 256] #:max-cores [len 144])
  (set! capacity c)
  (set! max-cores len)
  (core (make-vector len 0) (make-vector len (set)))
  )

(define (cores-count cores)
  (let ([v (core-space cores)])
    (apply +
           (for/list ([i (in-range 0 max-cores)])
             (if (= (vector-ref v i) 0) 0 1)))))

(define (cores-assert cores)
  (assert (<= (cores-count cores) max-cores))
  (let ([space (core-space cores)])
    (for ([i (in-range 0 max-cores)])
      (assert (<= (vector-ref space i) capacity) `cores-assert)
      )))

(define (cores-inc-space cores i add-space)
  (let ([space (core-space cores)])
       (if (symbolic? i)                     ; <-- optimization
           (let ([len max-cores])
             (assert (<= 0 i) `(<= 0 i))
             (assert (< i len) `(< i len))
             (for ([j (in-range 0 len)])
               (let ([val-space (vector-ref space j)])
                 (vector-set! space j (if (= i j) 
                                          (let ([new-space (+ val-space add-space)])
                                            (assert (<= new-space capacity)
                                                    `(<= new-space capacity))
                                            new-space)
                                          val-space)))))
           (let* ([val-space (vector-ref space i)] 
                  [new-space (+ val-space add-space)]) ; <-- optimization
             (pretty-display `(cores-inc-space ,i ,add-space))
             (assert (<= new-space capacity) `(<= new-space capacity))
             (vector-set! space i new-space))))
  ;(assert (<= (cores-count cores) max-cores))
  )

(define (cores-add-op cores i op)
  (define add-space (est-space op))
  (if (> add-space 4)
      (let* ([space (core-space cores)]
             [costly-op (core-costly-op cores)])
           (if (symbolic? i)                     ; <-- optimization
               (let ([len max-cores])
                 (assert (<= 0 i) `(<= 0 i))
                 (assert (< i len) `(< i len))
                 (for ([j (in-range 0 len)])
                   (let* ([val-space (vector-ref space j)]
                          [val-ops (vector-ref costly-op j)])
                     (vector-set! space j (if (= i j) 
                                              (let* ([more-space (if (set-member? val-ops op) 4 add-space)]
                                                     [new-space (+ val-space more-space)])
                                                (assert (<= new-space capacity)
                                                        `(<= new-space capacity))
                                                new-space)
                                              val-space))
                     (vector-set! costly-op j (if (= i j) 
                                                  (set-add val-ops op)
                                                  val-ops)))))
               (let* ([val-space (vector-ref space i)]
                      [val-ops (vector-ref costly-op i)]
                      [more-space (if (set-member? val-ops op) 4 add-space)]
                      [new-space (+ val-space more-space)])  ; <-- optimization
                 (assert (<= new-space capacity) `(<= new-space capacity))
                 (vector-set! space i new-space)
                 (vector-set! costly-op i (set-add val-ops op))
                 ))
           ;(assert (<= (cores-count cores) max-cores))
        )
      (cores-inc-space cores i add-space)))
