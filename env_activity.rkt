#lang racket

(define (new-environment) (list))

(define (add-binding E N V) (cons (cons N V) E))

(define (lookup-name E N)
  (if (empty? E)
      (error (~a "variable " N " not found"))
      (let* ([first-binding (first E)]
            [first-name (car first-binding)]
            [first-value (cdr first-binding)])
       (if (equal? first-name N)
          first-value
          (lookup-name (rest E) N)))))