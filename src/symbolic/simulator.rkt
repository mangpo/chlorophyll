#lang s-exp rosette

(require "ops-rosette.rkt" 
         "../arrayforth.rkt")

(require (only-in racket [equal? racket/equal?]))

(provide get-progstate analyze-reg-b)
 
(define bit 18)

(define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
(define-syntax-rule (modulo+ x y) (if (>= x 8) (- x y) x))

(struct stack (sp body))
(struct progstate (a b r s t data return memory recv comm) 
        #:mutable #:transparent)

;; Get item i from the stack
(define (get-stack stack i)
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))

(define (get-sym)
  (define-symbolic* val (bitvector bit))
  val)

(define (get-sym-bool)
  (define-symbolic* val boolean?)
  val)

(define (get-progstate mem)
  (progstate (get-sym) (get-sym) (get-sym) (get-sym) (get-sym)
             (stack 0 (for/vector ([i 8]) (get-sym)))
             (stack 0 (for/vector ([i 8]) (get-sym)))
             (for/vector ([i mem]) (get-sym))
             #f #f))


(define debug #t)

(define UP #x145)
(define DOWN #x115)
(define LEFT #x175)
(define RIGHT #x1d5)
(define IO #x15d)

(define (bitwise-and a b)
  (pretty-display `(bvand))
  (unless (bv? a) (set! a (bv a bit)))
  (unless (bv? b) (set! b (bv b bit)))
  (bvand a b))

(define (bitwise-ior a b)
  (pretty-display `(bvor))
  (unless (bv? a) (set! a (bv a bit)))
  (unless (bv? b) (set! b (bv b bit)))
  (bvor a b))

(define (bitwise-xor a b)
  (pretty-display `(bvxor))
  (unless (bv? a) (set! a (bv a bit)))
  (unless (bv? b) (set! b (bv b bit)))
  (bvxor a b))

(define (bitwise-not a)
  (pretty-display `(bvnot))
  (unless (bv? a) (set! a (bv a bit)))
  (bvnot a))

(define (bv+ a b)
  (unless (bv? a) (set! a (bv a bit)))
  (unless (bv? b) (set! b (bv b bit)))
  (bvadd a b))

(define (bv* a b)
  (unless (bv? a) (set! a (bv a bit)))
  (unless (bv? b) (set! b (bv b bit)))
  (bvmul a b))


(define (finitize num bit)
  (if (term? num)
      num
      (let* ([mask (arithmetic-shift -1 bit)]
             [masked (bitwise-and (bitwise-not mask) num)])
        (if (equal? (bitwise-and masked (arithmetic-shift 1 (sub1 bit))) 0)
            masked
            (bitwise-ior mask masked)))))

    ;; Interpret a given program from a given state.
    ;; code
    ;; state: initial progstate
    ;; policy: a procedure that enforces a communication policy (see the definition of comm-policy above)
    (define (analyze-reg-b code state id)

      (define func-map (make-hash))
      (define recur-map (make-hash))
      (define current-scope #t)
      
      (define a (progstate-a state))
      (define b (progstate-b state))
      (define r (progstate-r state))
      (define s (progstate-s state))
      (define t (progstate-t state))
      (define data-sp (stack-sp (progstate-data state)))
      (define data-body (vector-copy (stack-body (progstate-data state))))
      (define return-sp (stack-sp (progstate-return state)))
      (define return-body (vector-copy (stack-body (progstate-return state))))
      (define memory (vector-copy (progstate-memory state)))
      
      (define recv (progstate-recv state))
      (define comm (progstate-comm state))

      ;; Pushes a value to the given stack's body.
      (define-syntax-rule (push-stack! x-sp x-body value)
	(begin
	  (set! x-sp (modulo+ (add1 x-sp) 8))
	  (vector-set! x-body x-sp value)
	  ))

      ;; Pops from the given stack's body.
      (define-syntax-rule (pop-stack! x-sp x-body)
	(let ([ret-val (vector-ref x-body x-sp)])
	  (set! x-sp (modulo- (sub1 x-sp) 8))
	  ret-val))

      ;; Pushes to the data stack.
      (define (push! value)
	(push-stack! data-sp data-body s)
	(set! s t)
	(set! t value))
      
      ;; Pushes to the return stack.
      (define (r-push! value)
	(push-stack! return-sp return-body r)
	(set! r value))
      
      ;; Pops from the data stack.
      (define (pop!)
	(let ([ret-val t])
	  (set! t s)
	  (set! s (pop-stack! data-sp data-body))
	  ret-val))
      
      ;; Pops from the return stack.
      (define (r-pop!)
	(let ([ret-val r])
	  (set! r (pop-stack! return-sp return-body))
          ret-val))
      
      ;; Read from the given memory address or communication port. If it
      ;; gets a communication port, it just returns a random number (for
      ;; now).
      (define (read-memory addr)
        (get-sym))
      
      ;; Write to the given memeory address or communication
      ;; port. Everything written to any communication port is simply
      ;; aggregated into a list.
      (define (set-memory! addr val)
        (void))

      (define (clip x) (finitize x bit))

      (define (push-right-one x carry)
	(clip (bitwise-ior (<< (bitwise-and #x1 carry) (sub1 bit) bit)
                           (>>> x 1 bit))))
      
      ;; Treats T:A as a single 36 bit register and shifts it right by one
      ;; bit. The most signficicant bit (T17) is kept the same.
      (define (multiply-step-even!)
	(let ([a-val (push-right-one a t)]
	      [t-val (>> t 1 bit)])
	  (set! a a-val)
	  (set! t t-val)))
      
      ;; Sums T and S and concatenates the result with A, shifting
      ;; the concatenated 37-bit to the right by one bit.
      (define (multiply-step-odd!)
	(let* ([sum (bv+ t s)]
	       [a-val (push-right-one a sum)]
	       [t-val (>> sum 1 bit)])
	  (set! a a-val)
	  (set! t t-val)))

      (define-syntax-rule (mem-to-stack addr)
	(push! (read-memory addr)))

      (define-syntax-rule (stack-to-mem addr)
        (set-memory! addr (pop!)))

      (define-syntax-rule (stack-1 f)
        (set! t (f t)))

      (define-syntax-rule (stack-2 f)
	(let* ([val1 (pop!)]
               [val2 (pop!)])
          (push! (f val1 val2))))

      (define rm #f)

      (define (check-b-set! val)
        (when debug (pretty-display `(check-b ,(racket/equal? b val))))
        (when (racket/equal? b val)
          (set! rm #t))
        (set! b val)
        (when debug (pretty-display `(rm ,rm)))
        )

      (define (interpret-step inst)
	(when debug (pretty-display `(interpret-step ,inst)))
	(define-syntax-rule (inst-eq x) (equal? inst x))
	(cond
	 [(string->number inst)   (push! (string->number inst))]
	 [(inst-eq "io")    (push! IO)]
	 [(inst-eq "up")    (push! UP)]
	 [(inst-eq "down")  (push! DOWN)]
	 [(inst-eq "left")  (push! LEFT)]
	 [(inst-eq "right") (push! RIGHT)]
	 [(inst-eq "@+")   (mem-to-stack a)
	                   (set! a (add1 a))]
	 [(inst-eq "@b")   (mem-to-stack b)]
	 [(inst-eq "@")    (mem-to-stack a)]
	 [(inst-eq "!+")   (stack-to-mem a)
	                  (set! a (add1 a))]
	 [(inst-eq "!b")   (stack-to-mem b)]
	 [(inst-eq "!")    (stack-to-mem a)]
	 [(inst-eq "+*")   (if (= (bitwise-and #x1 a) 0)
	 		      (multiply-step-even!)
	 		      (multiply-step-odd!))];
	 [(inst-eq "2*")   (stack-1 (lambda (t) (clip (<< t 1 bit))))]
	 [(inst-eq "2/")   (stack-1 (lambda (t) (>> t 1 bit)))];; sign shiftx
	 [(inst-eq "-")    (stack-1 bitwise-not)]
	 [(inst-eq "+")    (stack-2 (lambda (x y) (clip (bv+ x y))))]
	 [(inst-eq "and")  (stack-2 bitwise-and)]
	 [(inst-eq "or")   (stack-2 bitwise-xor)]
	 [(inst-eq "drop") (pop!)]
	 [(inst-eq "dup")  (push! t)]
	 [(inst-eq "pop")  (push! (r-pop!))]
	 [(inst-eq "over") (push! s)]
	 [(inst-eq "a")    (push! a)]
	 [(inst-eq "nop")  (void)]
	 [(inst-eq "push") (r-push! (pop!))]
	 [(inst-eq "b!")   (check-b-set! (pop!))]
	 [(inst-eq "a!")   (set! a (pop!))]
	 [else (assert #f (format "invalid instruction ~a" inst))])
	 )

      (define (interpret-struct x)
	(when debug (pretty-display `(interpret-struct ,x)))
	(cond
	 [(string? x)
          (interpret-step x)]

	 [(or (vector? x) (list? x))
	  (for ([i x]) (interpret-struct i))]
	 
	 [(block? x)
          (define body
            (let ([b (block-body x)])
              (if (string? b)
                  (string-split b)
                  b)))
          
          (set! rm #f)
          (when (null? (block-rm x))
            (set-block-rm! x #t))
	  (interpret-struct body)
          
          (when (and (> (length body) 1)
                     (findf (lambda (x) (equal? x "b!")) body)
                     #;(or (member (first body)
                                 (list "up" "down" "left" "right" "io"))
                         (string->number (first body))))
            (set-block-rm! x (and (block-rm x) rm))
            (when debug (pretty-display `(set-block-rm! ,(block-rm x))))
            )
          ]

	 [(forloop? x)
	  (interpret-struct (forloop-init x))
	  (r-push! (pop!))
          (define bound (min (add1 r) 4))
          (when (term? bound) (set! bound 4))
	  (for ([i (in-range bound)])
	       (interpret-struct (forloop-body x))
	       (set! r (sub1 r)))
	  (r-pop!)
	  ]

	 [(ift? x)
	  ;(when (not (equal? t 0))
          (interpret-struct (ift-t x))]

	 [(iftf? x)
	  (if (get-sym-bool)
              (interpret-struct (iftf-t x))
              (interpret-struct (iftf-f x)))]

	 [(-ift? x)
	  ;(when (negative? t) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
          (interpret-struct (-ift-t x))]

	 [(-iftf? x)
	  (if (get-sym-bool) ;(or (negative? t) (>= t (arithmetic-shift 1 (sub1 bit)))) ;; negative
	      (interpret-struct (-iftf-t x))
	      (interpret-struct (-iftf-f x)))]

         [(vardecl? x)
          ;(vector-copy memory 0 (list->vector (vardecl-val x)))
          (void)
          ]

         [(port-exec? x) (set! b (get-sym))]
         [(port-listen? x) (void)]

         [(mult? x)
          (stack-2 (lambda (x y) (clip (bv* x y))))]

         [(abs? x)
          (void)]

         [(funccall? x)
          (define prev-scope current-scope)
          (define name (funccall-name x))
          (pretty-display `(st ,s ,t))
          (cond
            [(equal? name "*.17")
             (set! t (clip (>> (bv* s t) 17 bit)))]
            
            [(equal? name "*.")
             (set! t (clip (>> (bv* s t) 16 bit)))]
            
            [(equal? name "--u/mod") ;; TODO: make this precise
             (pop!) (pop!) (pop!)
             (push! (get-sym)) (push! (get-sym))]

            [(equal? name "out") (pop!)]
            [(equal? name "in") (push! (get-sym))]
            
            [(equal? current-scope name)
             ;; For recursive call, limit to one depth.
             (if (hash-ref recur-map name)
                 (hash-set! recur-map name #f)
                 (begin
                   (hash-set! recur-map name #t)
                   (set! current-scope name)
                   (interpret-struct (hash-ref func-map name))))]
            [else
             (set! current-scope name)
             (interpret-struct (hash-ref func-map name))])
          (set! current-scope prev-scope)
          ]

         [(funcdecl? x)
          (hash-set! func-map (funcdecl-name x) (funcdecl-body x))
          (hash-set! recur-map (funcdecl-name x) #f)]

         [(aforth? x)
          (interpret-struct (aforth-code x))
          (when (hash-has-key? func-map "main")
            (set! current-scope "main")
            (interpret-struct (hash-ref func-map "main")))
          (define act (format "act~a" id))
          (when (hash-has-key? func-map act)
            (set! current-scope act)
            (interpret-struct (hash-ref func-map act)))
          ]


	 [else (raise (format "interpret-struct: unimplemented for ~a" x))]
	 ))
      
      (interpret-struct code)

      (progstate a b r s t 
		  (stack data-sp data-body)
		  (stack return-sp return-body)
		  memory recv comm)
      )

