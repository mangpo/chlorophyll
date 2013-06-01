#lang s-exp rosette

(require 
 (rename-in rosette/lang/assert [assert* assert])
 rosette/base/bool rosette/base/value 
 rosette/lang/debug rosette/util/array)

(define (get-sym)
  (define-symbolic a number?)
  a)

(synthesize #:forall (list (get-sym) (get-sym))
            #:assume #t
            #:guarantee #t)

(current-solution)

(define-symbolic b number?)
(define-symbolic c number?)
(equal? b c)