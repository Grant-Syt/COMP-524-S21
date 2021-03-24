#lang racket

(define (eval-addition addition-expr)
  ;; addition := expr PLUS expr
  (let ([label          (first  addition-expr)]
        [left-expr      (second addition-expr)]
        [plus-token (third  addition-expr)]
        [right-expr     (fourth addition-expr)])
    (+ (eval-expr left-expr)
       (eval-expr right-expr))))