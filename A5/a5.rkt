#lang racket

;; import parse
(require (only-in (file "a3.rkt") parse))

;; Eval function
;;
;; Evaluates the result of the given string of source code
(define (eval code)
  (eval-program (parse code)))

;; program := exprList
;;
;; return value of last expression in exprList
(define (eval-program program-expr)
  (let* ([exprList (second program-expr)]
         [result-list (eval-exprList exprList)])
    ;; find last value
    (letrec ([helper
              (lambda (list)
                (if (null? (rest list))
                    (first list)
                    (helper (rest list))))])
      (helper result-list))))

;; exprList := expr optExprList
;;
;; return pair containing value of expr and value of optExprList
(define (eval-exprList exprList-expr)
  (let ([expr-expr (second exprList-expr)]
        [optExprList-expr (third exprList-expr)])
    (cons (eval-expr expr-expr)
          (eval-optExprList optExprList-expr))))

;; optExprList := ɛ | exprList
;;
;; optExprList is either an exprList pair or null
(define (eval-optExprList optExprList-expr)
  (if (null? (rest optExprList-expr))
      '()
      (eval-exprList (second optExprList-expr))))

;; expr := atom | invocation
(define (eval-expr expr-expr)
  (let ([label (first (second expr-expr))]
        [atom-expr (second expr-expr)])
  (if (equal? label 'atom)
      (eval-atom atom-expr)
      (eval-invocation atom-expr))))

;; atom := NAME | STRING | number
(define (eval-atom atom-expr)
  (let* ([content (second atom-expr)]
         [label (first content)])
    (if (equal? label 'NAME)
        (get-proc (second content))
        (if (equal? label 'STRING)
            (second content)
            (eval-number content)))))

;; number := INT | FLOAT
(define (eval-number number-expr)
  (second (second number-expr)))

;; invocation := OPAREN exprList CPAREN
(define (eval-invocation invocation-expr)
  (let ([rator (eval-expr (second (third invocation-expr)))]
        [optExprList-expr (third (third invocation-expr))])
    (if (equal? rator 'and)
        (eval-and2 optExprList-expr)
        (if (equal? rator 'or)
            (eval-or2 optExprList-expr)
            (let* ([values (eval-exprList (third invocation-expr))]
                   [rator (first values)]
                   [rands (rest values)])
              (apply rator rands))))))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Helper functions ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (get-proc name)
  (cond
    [(equal? name '+) +]
    [(equal? name '-) -]
    [(equal? name '*) *]
    [(equal? name '/) /]
    [(equal? name 'string-append) string-append]
    [(equal? name 'string<?) string<?]
    [(equal? name 'string=?) string=?]
    [(equal? name 'not) not]
    [(equal? name '=) =]
    [(equal? name '<) <]
    [(equal? name 'and) 'and]
    [(equal? name 'or) 'or]
    [else (error "unknown name")]))

(define (eval-and optExprList-expr)
  (let ([len (optExprList-length optExprList-expr)])
    (cond
      [(equal? len 0) #t]
      [(equal? len 1) (eval-expr (next-expr optExprList-expr))]
      [else (letrec
                ([helper
                  (lambda (optExprList-expr)
                    (if (null? (rest optExprList-expr))
                        #t
                        (if (equal? (eval-expr (next-expr optExprList-expr))
                                    #f)
                            #f
                            (helper (next-optExprList optExprList-expr)))))])
              (helper optExprList-expr))])))

(define (eval-or optExprList-expr)
  (let ([len (optExprList-length optExprList-expr)])
    (cond
      [(equal? len 0) #f]
      [(equal? len 1) (eval-expr (next-expr optExprList-expr))]
      [else (letrec
                ([helper
                  (lambda (optExprList-expr)
                    (if (null? (rest optExprList-expr))
                        #f
                        (if (equal? (eval-expr (next-expr optExprList-expr))
                                    #t)
                            #t
                            (helper (next-optExprList optExprList-expr)))))])
              (helper optExprList-expr))])))

(define (optExprList-length optExprList-expr)
  (letrec ([helper
            (lambda (count optExprList-expr)
              (if (null? (rest optExprList-expr))
                  count
                  (helper (+ count 1)
                          (third (second optExprList-expr)))))])
    (helper 0 optExprList-expr)))

(define (next-expr optExprList-expr)
  (second (second optExprList-expr)))

(define (next-optExprList optExprList-expr)
  (third (second optExprList-expr)))

(define (opt-to-expr optExprList-expr)
  (letrec ([helper
            (lambda (ls optExprList-expr)
              (if (null? (rest optExprList-expr))
                  ls
                  (helper (append
                           ls
                           (list (next-expr optExprList-expr)))
                          (next-optExprList optExprList-expr))))])
    (helper '() optExprList-expr)))

(define (eval-and2 optExprList-expr)
  (andmap eval-expr (opt-to-expr optExprList-expr)))

(define (eval-or2 optExprList-expr)
  (ormap eval-expr (opt-to-expr optExprList-expr)))