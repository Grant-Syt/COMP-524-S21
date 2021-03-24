#lang racket

;; import lexing function
(require (only-in (file "lex.rkt") lex))

;; export
(provide parse)

;; define tokens as a parameter (dynamic scope)
(define tokens (make-parameter null))

;; parse function
;;
;; returns a parse tree given a string of source code
(define (parse code)
  (parameterize ([tokens (lex code)])
    (let ([tree (parse-program)])
      (if (empty? (tokens))
          tree
          (error "unconsumed tokens")))))

;;;;;;;;;;;;;;;;;;;;;;;
;; parsing functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; program := exprList
(define (parse-program)
  (list 'program (parse-exprList)))

;; exprList := expr optExprList
(define (parse-exprList)
  (list 'exprList (parse-expr) (parse-optExprList)))

;; optExprList := ɛ | exprList
(define (parse-optExprList)
  (if (exprList-pending?)
      (list 'optExprList (parse-exprList))
      (list 'optExprList)))

;; expr := atom | invocation | let | define | lambda
(define (parse-expr)
  (cond
    [(atom-pending?) (list 'expr (parse-atom))]
    [(invocation-pending?) (list 'expr (parse-invocation))]
    [(let-pending?) (list 'expr (parse-let))]
    [(define-pending?) (list 'expr (parse-define))]
    [(lambda-pending?) (list 'expr (parse-lambda))]))

;; let := LET OPAREN NAME expr CPAREN expr
(define (parse-let)
  (list 'let
        (consume 'LET)
        (consume 'OPAREN)
        (consume 'NAME)
        (parse-expr)
        (consume 'CPAREN)
        (parse-expr)))

;; define := DEFINE NAME expr
(define (parse-define)
  (list 'define
        (consume 'DEFINE)
        (consume 'NAME)
        (parse-expr)))

;; lambda := LAMBDA OPAREN NAME CPAREN expr
(define (parse-lambda)
  (list 'lambda
        (consume 'LAMBDA)
        (consume 'OPAREN)
        (consume 'NAME)
        (consume 'CPAREN)
        (parse-expr)))

;; atom := NAME | STRING | number
(define (parse-atom)
  (if (check 'NAME)
      (list 'atom (consume 'NAME))
      (if (check 'STRING)
          (list 'atom (consume 'STRING))
          (when (number-pending?) ;; could remove*************
              (list 'atom (parse-number))))))

;; number := INT | FLOAT
(define (parse-number)
  (if (check 'INT)
      (list 'number (consume 'INT))
      (list 'number (consume 'FLOAT))))

;; invocation := OPAREN exprList CPAREN
(define (parse-invocation)
  (list 'invocation
        (consume 'OPAREN)
        (parse-exprList)
        (consume 'CPAREN)))

;; consume function
;;
;; given a token type, checks for two errors, otherwise returns the first token
;; and updates the token list
(define (consume type)
  (when (empty? (tokens))
    (error (~a "expected token of type " type " but no remaining tokens")))
  (let ([token (first (tokens))])
    (when (not (equal? type (first token)))
      (error (~a "expected token of type " type " but actual token was " token)))
    (tokens (rest (tokens)))  ; update tokens: remove first token
    token))

;; check function
;;
;; check the type of the next token without consuming it
(define (check type)
  (if (empty? (tokens))
      #f
      (equal? type (first (first (tokens))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; pending? functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(define (exprList-pending?) (expr-pending?))

(define (expr-pending?)
  (or (atom-pending?)
      (invocation-pending?)
      (let-pending?)
      (define-pending?)
      (lambda-pending?)))

(define (let-pending?) (check 'LET))

(define (define-pending?) (check 'DEFINE))

(define (lambda-pending?) (check 'LAMBDA))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

(define (invocation-pending?) (check 'OPAREN))