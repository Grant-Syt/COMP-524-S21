#lang racket
(define (update-binding env name value)
  (if (hash-has-key? env name)
      (hash-set env name value)
      (error 'exception)))

(define (new-counter)
  (let ([counter -1])
    (lambda ()
      (set! counter (add1 counter))
      counter)))
