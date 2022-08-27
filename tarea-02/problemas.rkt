#lang racket

(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

(define (take ls n)
  (cond
    [(or (equal? n 0) (empty? ls)) null]
    [else (cons (first ls) (take (rest ls) (sub1 n)))]
  ))

(define (drop ls n)
  (cond
    [(or (equal? n 0) (empty? ls)) ls]
    [else (drop (rest ls) (sub1 n))]
  ))


(define (bundle s n)
  (cond
    [(or (null? s) (or (zero? n) (< n 0))) s]
    [else
     (cons (implode (take s n))
           (bundle (drop s n) n))]))

(provide (all-defined-out))