#lang racket

(define def-re #rx"^[a-zA-Z0-9]+@live\\.unc\\.edu$")

(define (run-re input)
  (regexp-match def-re input))

