#lang racket
(define (eval-nif nif-expr env)
  ;; nif := NIF expr expr expr expr‌‍‎‌‏
  (let* ([val (eval-expr(third nif-expr))]
         [one-expr (fourth nif-expr)]
         [two-expr (fifth nif-expr)]
         [three-expr (sixth nif-expr)])
    (if (< val 0)
        (eval-expr one-expr)
        (if (= val 0)
            (eval-expr two-expr)
            (if (> val 0)
                (eval-expr three-expr)
                -1)))))

(let ([ls1 (list