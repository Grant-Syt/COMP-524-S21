#lang racket

(define (eval-definition def-expr optExpr-expr env)
  ;; definition := DEFINE NAME EQ expr SEMI​‎‏‏
  (let* ([name-token (third def-expr)]
         [name (second name-token)]
         [val-expr (fifth def-expr)]
         [value (eval-expr val-expr env)])
    (eval-optExpr optExpr-expr
               (add-binding env name value))))