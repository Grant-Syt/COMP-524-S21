#lang racket

(define tree
  '(expr
    (OPAREN #f)
    (expr
     (OPAREN #f)
     (expr (NUM 3))
     (op (PLUS #f))
     (expr (NUM 5))
     (CPAREN #f))
    (op (TIMES #f))
    (expr (NUM 4))
    (CPAREN #f)))
