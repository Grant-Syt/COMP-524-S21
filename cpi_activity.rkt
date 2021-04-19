#lang racket

;; provided solution
(define (make-plus-cont1 env cont val2-expr)
  (lambda (val1)
    (eval-expr val2-expr env (make-plus-cont2 env cont val1))))

(define (make-plus-cont2 env cont val1)‌‍‎‌‏
‌‍‎‌‏  (lambda (val2)‌‍‎‌‏
‌‍‎‌‏  ‌‍‎‌‏  (cont (+ val1 val2))))‌‍‎‌‏
‌‍‎‌‏
(define (eval-plus plus-expr env cont)‌‍‎‌‏
‌‍‎‌‏  ;; plus := OPAREN PLUS expr expr CPAREN‌‍‎‌‏
‌‍‎‌‏  (let* ([val1-expr (third plus-expr)]‌‍‎‌‏
‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏   [val2-expr (fourth plus-expr)])‌‍‎‌‏
‌‍‎‌‏  ‌‍‎‌‏  (eval-expr val1-expr env‌‍‎‌‏
‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏  ‌‍‎‌‏   (make-plus-cont1 env cont val2-expr))))‌‍‎‌‏

;; diagram
(+ 1 2)
(eval-expr val1-expr env
           (lambda (val1) (eval-expr val2-expr env
                                     (lambda (val2) (cont (+ val1 val2))))))
(eval-expr val2-expr env
           (lambda (val2) (cont (+ 1 val2))))
(cont (+ 1 2))
(cont 3)