#lang racket

;; import lexing function
(require (only-in (file "lex.rkt") lex))

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

(define (parse-program)
  (list 'program (parse-exprList)))

(define (parse-exprList)
  (list 'exprList (parse-expr) (parse-optExprList)))

(define (parse-optExprList)
  (if (exprList-pending?)
      (list 'optExprList (parse-exprList))
      (list 'optExprList)))

(define (parse-expr)
  (if (atom-pending?)
      (list 'expr (parse-atom))
      (list 'expr (parse-invocation))))

(define (parse-atom)
  (if (check 'NAME)
      (list 'atom (consume 'NAME))
      (if (check 'STRING)
          (list 'atom (consume 'STRING))
          (when (number-pending?) ;; could remove*************
              (list 'atom (parse-number))))))

(define (parse-number)
  (if (check 'INT)
      (list 'number (consume 'INT))
      (list 'number (consume 'FLOAT))))

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

(define (exprList-pending?)
  (or (atom-pending?)
      (invocation-pending?)))

(define (atom-pending?)
  (or (check 'NAME)
      (check 'STRING)
      (number-pending?)))

(define (invocation-pending?)
  (check 'OPAREN))

(define (number-pending?)
  (or (check 'INT)
      (check 'FLOAT)))

