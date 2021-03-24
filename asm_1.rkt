#lang racket

(define (number-items items)
  (letrec ([number-items-from
            (lambda (n items)
              (if (empty? items)
                  null
                  (cons (list n (car items))
                        (number-items-from (add1 n) (cdr items)))))])
    (number-items-from 0 items)))

(define (countdown n)
  (if (zero? (+ n 1))
      null
      (cons n
            (countdown (sub1 n)))))

(define (remove-first n items)
  (if (null? items)
      null
      (if (equal? n (first items))
          (rest items)
          (cons (first items)
                (remove-first n (rest items))))))

(define (insert-after-every a b items)
  (if (null? items)
      null
      (if (equal? a (first items))
          (cons a
                (cons b
                      (insert-after-every a b (rest items))))
          (cons (first items)
                (insert-after-every a b (rest items))))))

(define (zip list1 list2)
  (if (null? list1)
      null
      (if (null? list2)
          null
          (cons (list (first list1) (first list2))
                (zip (rest list1) (rest list2))))))

(define (append ls1 ls2)
  (if (null? ls1)
      (if (null? ls2)
          null
          (cons (first ls2) (append ls1 (rest ls2))))
      (cons (first ls1) (append (rest ls1) ls2))))

(define (binary->natural items)
  (letrec ([helper
            (lambda (total i items)
              (if (null? items)
                  total
                  (helper (+ total (* (expt 2 i) (first items)))
                          (+ i 1) (rest items))))])
    (helper 0 0 items)))

(define (index-of-item elem list)
  (letrec ([helper
            (lambda (i elem list)
              (if (equal? elem (first list))
                  i
                  (helper (+ i 1) elem (rest list))))])
    (helper 0 elem list)))

(define (divide a b)
  (letrec ([helper
            (lambda (cnt a b)
              (if (zero? a)
                  cnt
                  (helper (+ cnt 1) (- a b) b)))])
    (helper 0 a b)))

(define (countdown2 n) (range n -1 -1))

(define (zip2 ls1 ls2)
  (map (λ (elem1 elem2)
         (list elem1 elem2))
       ls1 ls2))

(define (sum-of-products ls1 ls2)
  (foldl (λ (a b result)
           (+ result (* a b)))
         0 ls1 ls2))
