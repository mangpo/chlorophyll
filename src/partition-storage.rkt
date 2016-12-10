#lang s-exp rosette

(require "header.rkt"
         "space-estimator.rkt" "symbolic/ops-rosette.rkt")

(provide (all-defined-out))

(struct core (space costly-op))

(define capacity #f)
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

(define (cores-refine cores part2capacity)
  (pretty-display `(cores-refine ,part2capacity))

  (for ([pair (hash->list part2capacity)])
       (cores-inc-space cores (car pair) 0 (cdr pair))))

  ;; (pretty-display `(cores-refine ,refine-capacity ,part2sym))
  ;; (unless part2sym
  ;;         (set! part2sym (make-vector max-cores #f)))
  ;; (for ([i max-cores])
  ;;      (let ([part (vector-ref part2sym i)])
  ;;        (unless (equal? part #f)
  ;;              (cores-inc-space cores part 0
  ;;                               (vector-ref refine-capacity i))))))
       

(define (cores-count cores)
  (let ([v (core-space cores)]
        [all 0])
    (for ([i max-cores])
         (when (bv!= (vector-ref v i) 0)
               (set! all (bv+ all 1))))
    all))

(define (cores-inc-space cores i add-space [refine-capacity capacity])
  (let ([space (core-space cores)])
       (if (symbolic? i)                     ; <-- optimization
           (let ([len max-cores])
             (assert (bvu<= 0 i) `(<= 0 i))
             (assert (bvu< i (sub1 len)) `(< i len))
             (for ([j (in-range 0 len)])
               (let ([val-space (vector-ref space j)])
                 (vector-set! space j (if (bv= i j) 
                                          (let ([new-space (bv+ val-space add-space)])
                                            (assert (bvu<= new-space refine-capacity)
                                                    `(<= new-space capacity))
                                            new-space)
                                          val-space)))))
           (let* ([val-space (vector-ref space i)] 
                  [new-space (bv+ val-space add-space)]) ; <-- optimization
             ;(pretty-display `(cores-inc-space ,i ,add-space))
             (assert (bvu<= new-space refine-capacity) 
                     `(<= new-space capacity ,new-space))
             (vector-set! space i new-space))))
  ;(assert (<= (cores-count cores) max-cores))
  )
