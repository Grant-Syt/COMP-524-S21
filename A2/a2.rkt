#lang racket

;;;; Individual token storage
;;;;
;;;; The `[data #f]` is a default value for the 2nd positional argument.
;;;; That way, this function can take 1 arg or 2 args.
(define (token type [data #f])
  (list type data))

;;;; Token creator functions
;;;;
;;;; Given a string matching a regexp, return a token for that input or #f for
;;;; no token.

(define (skip-match str) #f)

(define (punctuation-token str)
  (token
    (case str
      [("(") 'OPAREN]
      [(")") 'CPAREN]
      [("{") 'OBRACE]
      [("}") 'CBRACE]
      [(",") 'COMMA]
      [(";") 'SEMICOLON]
      [(".") 'PERIOD])))

(define (int-token str)
  (token 'INT (string->number str)))

(define (float-token str)
  (token 'FLOAT (string->number str)))

(define (string-lit-token str)
  (begin (set! str (string-trim str "\""))
         (token 'STRING str)))

;; todo: add/replace keywords
(define (name-or-keyword-token str)
  (case str
    [("def" "fun" "if" "not" "and" "or")
     (token (string->symbol (string-upcase (string-trim str))))]
    [else (token 'NAME (string->symbol str))]))

;;;; Lexing rules table
;;;;
;;;; Each item in the table is a 2-tuple (i.e. list of 2 elements):
;;;; 1. a regexp to detect a token at the beginning of a string
;;;; 2. a function (from above) to take the matched string and create a token
(define re-table
  (list
    (list #rx"^[ \r\n\t]+" skip-match) ; whitespace
    (list #rx"^//[^\n]+(\n|$)" skip-match) ; // comments
    (list #rx"^/\\*.*?\\*/" skip-match) ; /*...*/ comments
    (list #rx"^[(){},;.]" punctuation-token)
    (list #rx"^-?[0-9]+(?=[\r\n\t (){},;.]|$)" int-token)
    (list #rx"^-?[0-9]+\\.[0-9]+(?=[\r\n\t (){},;.]|$)" float-token)
    (list #rx"^\".*?\"(?=[\r\n\t (){},;.]|$)" string-lit-token)
    (list #rx"^[^0-9(){},;.\" \r\n\t][^(){},;.\" \r\n\t]*(?=[\r\n\t (){},;.]|$)"
          name-or-keyword-token)))

;;;; Get matches helper function
;;;;
;;;; returns next match from input string
(define (get-matches input)
  (map (lambda (entry)
         (regexp-match (first entry) input))
       re-table))

;;;; Get longest match helper function
;;;;
;;;; returns index, length, and data of longest match given list of matches
(define (max-match matches)
  (letrec ([helper
            (lambda (i max_i max_l rem_matches matches)
              (define curr_match null)
              (if (null? rem_matches) ;; base case
                  (list
                   max_i
                   max_l
                   (list-ref matches max_i)) 
                  (begin (set! curr_match (first rem_matches))
                   (if (equal? curr_match #f) ;; ignore #f matches
                       (helper (+ i 1) max_i max_l (rest rem_matches) matches)
                       (begin (set! curr_match (first curr_match)) ;; use string
                              (if (> ;; compare actual matches
                                   (string-length curr_match)
                                   max_l)
                                  (begin (set! max_i i) ;; new max
                                         (set! max_l (string-length curr_match))
                                         (helper (+ i 1) max_i max_l
                                                 (rest rem_matches) matches))
                                  (helper (+ i 1) max_i max_l
                                          (rest rem_matches) matches)))))))])
    (helper 0 0 0 matches matches)))

;;;; Get next token helper function
;;;;
;;;; returns list with next token or null and remaining input given input string
(define (next-token input)
  (define curr_match null)
  (letrec ([helper
            (lambda (input)
              (if (equal? input "") (list null "") ;; base case
                  ;; else try to find match
                  (begin (set! curr_match (max-match (get-matches input)))     
                         ;; check if max string is #f
                         (if (equal? (list-ref curr_match 2) #f)
                             ;; return invalid input
                             (list (token 'INVALID input) "") 
                             ;; check if token can be made
                             (if (< (list-ref curr_match 0) 3)
                                 ;; ignore whitespace and comments
                                 ;; recur
                                 (helper (string-trim
                                          input
                                          (first (list-ref curr_match 2))
                                          #:right? #f))
                                 ;; else return token and remaing input
                                 (list
                                  ;; token
                                  (
                                   ;; token function
                                   (list-ref
                                    ;; re-table entry
                                    (list-ref re-table
                                              ;; max_i
                                              (list-ref curr_match 0)) 1)
                                   ;; max string
                                   (first (list-ref curr_match 2)))
                                  ;; remaining input
                                  (string-trim
                                   input
                                   (first (list-ref curr_match 2))
                                   #:right? #f)))))))])
    (helper input)))

;;;; Lexer
;;;;
;;;; Function receives a string and returns a list of recognized tokens in
;;;; the order that they were encountered in the string.
(define (lex input)
  (define curr_res null)
  (letrec ([helper
            (lambda (input output)
              (if (equal? input "") output ;; base case
                  ;; else get next token
                  (begin (set! curr_res (next-token input))
                         ;; input empty after calling next token
                         (if (null? (first curr_res))
                             output
                             ;; else add next match to output and recur
                             (begin (set! output (append output (list (first curr_res))))
                                    (helper (list-ref curr_res 1) output))))))])
    (helper input null)))