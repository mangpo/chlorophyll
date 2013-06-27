#lang s-exp rosette

(require "nmap.rkt")

(provide (all-defined-out))

; The layout will be represented by an nmap.
; There will be just one key - the function ID
; The values will be the estimated size, the physical core it lives
; on, and the dependencies of the function.
; Estimated size is just a number.
; Physical core is defined in physical-core.rkt
; Dependencies is a list of pairs.  Each pair is a pair of a physical
; core and a number, representing how many times the other physical
; core sends a message to this core.
(define (make-layout-map)
  (make-nmap 1))

(define val->id car)
(define val->size cadr)
(define val->core caddr)
(define val->dependencies cadddr)

(define add-layout-entry! add-ntuple!)

(define (id->val layout-map id)
  (get-by-nth-key layout-map 0 id))

(define (core->vals layout-map core)
  (get-list-by-nth-val layout-map 2 core))

(define (all-cores layout-map)
  (set->list (apply set (map caddr (get-all-values layout-map)))))
