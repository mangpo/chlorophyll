#lang racket

(require "visitor-comminsert.rkt")

(provide (all-defined-out))

(define (insert-comm ast routing-table part2core)
  (define commcode-inserter (new commcode-inserter% 
                                 [routing-table routing-table]
                                 [part2core part2core]))

  (send ast accept commcode-inserter))