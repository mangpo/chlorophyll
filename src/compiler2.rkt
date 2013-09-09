#lang racket

(require "parser.rkt"
         "compiler.rkt"
         "partitioner.rkt")

(define (optimize-file file cores capacity)
  (define my-ast (parse file))
  (optimize-comm my-ast #:cores cores #:capacity capacity #:verbose #t))


;(optimize-file "../examples/test.cll" 4 256)
(compile-to-IR "../examples/test.cll" "test" 256 "null" 2 2 
               #:verbose #t #:run #t)