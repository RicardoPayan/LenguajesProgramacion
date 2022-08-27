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

;Problema 3
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

;Problema 6
(define (list->chunks ls n)
  (cond
    [(empty? ls) null]
    [else (cons (take ls n) (list->chunks (drop ls n) n))]))

(define (bundle-chunk s n)
 (cond
   [(or (null? s) (or (zero? n) (< n 0))) s]
   [else (let add ([ls (list->chunks s n)])
           (cond
             [(empty? ls) null]
             [else (cons (implode (first ls)) (add (rest ls)))]))]))

(define (partition s n)
  (cond
    [(or (null? s) (or (zero? n) (< n 0))) s]
    [else (bundle (explode s) n)]))

(provide (all-defined-out))