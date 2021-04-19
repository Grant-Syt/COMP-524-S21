#lang racket

(define (parse-atom)
  (if (check 'NAME)
      (list 'atom (consume 'NAME))
      (if (check 'STRING)
          (list 'atom (consume 'STRING))
          (when (number-pending?) ;; could remove*************
              (list 'atom (parse-number))))))

(define (parse-with)
  (list 'with
        (consume 'WITH)
        (consume 'NAME)
        (consume 'EQ)
        (parse-expr)
        (consume 'OBRACE)
        (parse-expr)
        (consume 'CBRACE)))

(define (eval-with with-expr env)
  (let* ([name (second (third with-expr))]
         [val (eval-expr (fifth with-expr) env)]
         [env2 (add-binding env name val)]
         [body-expr (seventh with-expr)])
    (eval-expr body-expr env2)))