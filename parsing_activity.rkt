#lang racket

(define (parse-num-list-tail)
  (if (check 'COMMA)
      (list 'num_list_tail
            (consume 'COMMA)
            (consume 'NUMBER)
            (parse-num-list-tail))
      (list 'num_list_tail)))
