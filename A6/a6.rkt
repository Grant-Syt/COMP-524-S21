#lang racket

;; import parse
(require (only-in (file "parse.rkt") parse))

;; Environment implementation
(define new-environment hash)
(define add-binding hash-set)
(define lookup-name hash-ref)

;; Eval function
;;
;; Evaluates the result of the given string of source code
(define (eval code)
  (let* ([env (new-environment)]
         [env-funcs (add-funcs env)])
    (eval-program (parse code) env-funcs)))

;; program := exprList
;;
;; return value of last expression in exprList
(define (eval-program program-expr env)
  (let* ([exprList-expr (second program-expr)]
         [result-list (eval-exprList exprList-expr env)])
    ;; find last value
    (letrec ([helper
              (lambda (list)
                (if (null? (rest list))
                    (first list)
                    (helper (rest list))))])
      (helper result-list))))

;; exprList := expr optExprList
;;
;; returns pair containing value of expr and value of optExprList
(define (eval-exprList exprList-expr env)
  (let ([expr-expr (second exprList-expr)]
        [optExprList-expr (third exprList-expr)])
    (eval-expr expr-expr optExprList-expr env)))

;; optExprList := ɛ | exprList
;;
;; optExprList is either an exprList pair or null
(define (eval-optExprList optExprList-expr env)
  (if (null? optExprList-expr)
      null
      (if (null? (rest optExprList-expr))
          '()
          (eval-exprList (second optExprList-expr) env))))

;; expr := atom | invocation | let | define | lambda
;;
;; returns value pair (for eval-exprList)
;; paired with null if given null as optExprList-expr
(define (eval-expr expr-expr optExprList-expr env)
  (let* ([content (second expr-expr)]
         [label (first content)])
    (case label
      [(atom) (cons (eval-atom content env)
                    (eval-optExprList optExprList-expr env))]
      [(invocation) (cons (eval-invocation content env)
                          (eval-optExprList optExprList-expr env))]
      [(let) (cons (eval-let content env)
                   (eval-optExprList optExprList-expr env))]
      [(define) (eval-define content optExprList-expr env)]
      [(lambda) (cons (eval-lambda content env)
                      (eval-optExprList optExprList-expr env))]
      [else (error (~a "unknown expr type: " label))])))

;; let := LET OPAREN NAME expr CPAREN expr
(define (eval-let let-expr env)
  (let* ([name-token (fourth let-expr)]
         [name (second name-token)]
         [val-expr (fifth let-expr)]
         [body-expr (seventh let-expr)])
    (first
     (eval-expr body-expr null
                (add-binding env name
                             (first
                              (eval-expr val-expr null env)))))))

;; define := DEFINE NAME expr
;;
;; returns value pair (for eval-expr)
(define (eval-define define-expr optExprList-expr env)
  (let* ([name-token (third define-expr)]
         [name (second name-token)]
         [val-expr (fourth define-expr)]
         [val (first (eval-expr val-expr null env))])
    (cons val
          (eval-optExprList optExprList-expr
                      (add-binding env name val)))))

;; lambda := LAMBDA OPAREN NAME CPAREN expr
(define (eval-lambda lambda-expr env)
  (let* ([name-token (fourth lambda-expr)]
         [name (second name-token)]
         [body-expr (sixth lambda-expr)])
    (list name body-expr env)))

;; atom := NAME | STRING | number
(define (eval-atom atom-expr env)
  (let* ([content (second atom-expr)]
         [label (first content)])
    (case label
      [(NAME) (let ([name (second content)])
                (if (hash-has-key? env name)
                  (lookup-name env name)
                  (error (~a "unknown name: " name))))]
      [(STRING) (second content)]
      [(number) (eval-number content env)]
      [else (error (~a "unknown atom type: " label))])))

;; number := INT | FLOAT
(define (eval-number number-expr env)
  (second (second number-expr)))

;; invocation := OPAREN exprList CPAREN
;(define (eval-invocation invocation-expr env)
;  (let* ([values (eval-exprList (third invocation-expr) env)]
;         [rator (first values)]
;         [rands (rest values)])
;    (apply rator rands)))

(define (eval-invocation invocation-expr env)
  (let* ([values (eval-exprList (third invocation-expr) env)]
         [rator (first values)]
         [rands (rest values)])
    (if (list? rator)
        ;; procedure
        (let* ([procedure rator]
               [parameter (first procedure)]
               [body-expr (second procedure)]
               [lambda-env (third procedure)]
               [argument (first rands)])
          (first
           (eval-expr body-expr null
                      (add-binding lambda-env parameter argument))))
        ;; pre def func
        (apply rator rands))))

;;;;;;;;;;;;;;;;;;;;;;
;;                  ;;
;; Helper functions ;;
;;                  ;;
;;;;;;;;;;;;;;;;;;;;;;

(define (get-proc name)
  (case name
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [(string-append) string-append]
    [(string<?) string<?]
    [(string=?) string=?]
    [(not) not]
    [(=) =]
    [(<) <]
    [else (error (~a "unknown name: " name))]))

(define (add-funcs env)
  (let* ([env (add-binding env '+ +)]
         [env (add-binding env '- -)]
         [env (add-binding env '* *)]
         [env (add-binding env '/ /)]
         [env (add-binding env 'string-append string-append)]
         [env (add-binding env 'string<? string<?)]
         [env (add-binding env 'string=? string=?)]
         [env (add-binding env 'not not)]
         [env (add-binding env '= =)]
         [env (add-binding env '< <)])
    env))