#lang racket

;(define (g)
;  (let* ([x 3]
;        [y (f x)])
;    (+ x y)))
;
;(a (b (c) (d) (e)) (f (g (h) (i)) (j)) (k))​‎‏‏

(define (double-numbers list)
  (if (null? list)
      null
      (cons (first (* list 2))
            (double-numbers (rest list)))))

(define (list-product‎‏‏ list)
  (letrec ([helper
            (lambda (prod list)
              (if (null? list)
                  prod
                  (helper (* (first list) prod) (rest list))))])
    (helper 1 list)))

;(define re-table
;  (list
;   (list)))

;(define (re-table)
;  (list
;    (list #rx"^[ \r\n\t]+" skip-match)
;    (list #rx"^[0-9]+" int-t)
;    (list #rx"^[+\\-\\*/=]" op-t)
;    (list #rx"^[a-z]" name-t)
;    (list #rx"^[;]" semi-t)))

(define result
  '(disjunct
    (conjunct
     (conjunct
      (conjunct
       (atom
        (TRUE #f)))
      (AND #f)
      (atom
       (TRUE #f)))
     (AND #f)
     (atom
      (FALSE #f)))))

;(define (binding-lvalue-pending?)‎‏‏
;  (or (check NAME)
;      (seq-names-pending?‎‏‏)
;      (map-names-pending?‎‏‏)))

;(define (parse-binding-lvalue‎‏‏)
;  ;; binding-lvalue := NAME | seq-names | map-names‎‏‏
;  (if (check 'NAME)
;      (list 'binding-lvalue (consume 'NAME))
;      (if (seq-names-pending?)
;          (list 'binding-lvalue (parse-seq-names))
;          (list 'binding-lvalue (parse-map-names)))))

;(define (eval-arg-list arg-list-expr)
;  ;; arg-list‎‏‏ := arg arg-list-tail‎‏‏
;  (let* ([arg-expr (second arg-list-expr)]
;         [arg-val (eval-arg arg-expr)]
;         [arg-list-tail-expr (third arg-list-expr)])
;    (eval-arg-list-tail arg-val arg-list-tail-expr)))
;
;(define (eval-arg-list-tail arg-val arg-list-tail-expr)
;  ;; arg-list-tail := ε | COMMA arg-list
;  (if (null? (rest arg-list-tail-expr))
;      (list arg-val)
;      (cons arg-val
;            (eval-arg-list (third arg-list-tail-expr)))))

;(define (eval-let2 let2-expr env)
;  ;; let2 := LET2 NAME expr COMMA NAME expr IN expr
;  (let* ([name1 (second (third let2-expr))]
;         [value1 (eval-expr (fourth let2-expr) env)]
;         [name2 (second (sixth let2-expr))]
;         [value2 (eval-expr (seventh let2-expr) env)]
;         [new-env (add-binding env name1 value1)]
;         [new-env2 (add-binding new-env name2 value2)]
;         [body-expr (ninth let2-expr)])
;    (eval-expr body-expr new-env2)))

(define (lookup-name env name)
  (let ([names (first env)]
        [values (second env)])
    (if (empty? names)
        (error "not found")
        (if (equal? name (first names))
            (first values)
            (lookup-name (list (rest names)
                               (rest values))
                         name)))))