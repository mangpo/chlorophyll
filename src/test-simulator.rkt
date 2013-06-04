#lang s-exp rosette

(require "compiler.rkt" "communication.rkt")

(test-simulate "if" "4_1")
(test-simulate "add" "200")
(test-simulate "function" "4_1")
(test-simulate "function" "4_2")

;(test-simulate "cluster" "200")