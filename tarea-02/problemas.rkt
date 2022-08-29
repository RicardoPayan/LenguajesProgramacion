#lang racket
(require racket/trace)


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

;Problema 7
(define (partition s n)
  (cond
    [(or (null? s) (or (zero? n) (< n 0))) s]
    [else (bundle (explode s) n)]))

;Problema 8
(define (isort ls predicado)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) predicado) predicado)))
 

(define (insert n ls predicado)
  (cond
    [(empty? ls) (list n)]
    [(predicado n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) predicado))]))

;Problema 10

(define (smallers ls pivot)
  (cond
    [(empty? ls) null]
    [(< (first ls) (first pivot)) (append (list (first ls)) (smallers (rest ls) pivot))]
    [else (smallers (rest ls) pivot)]))

(define (largers ls pivot)
  (cond
    [(empty? ls) null]
    [(> (first ls) (first pivot)) (append (list (first ls)) (largers (rest ls) pivot))]
    [else (largers (rest ls) pivot)]))

(define (same ls pivot)
  (cond
    [(empty? ls) null]
    [(equal? (first ls) pivot) (cons (first ls) (same (rest ls) pivot))]
    [else (same (rest ls) pivot)]
    ))

(define (quicksort ls )
  (cond
    [(empty? ls) null]
    [else
     (define pivot (same ls (first ls)))
     (append (quicksort (smallers ls pivot)) (list pivot) (quicksort (largers ls pivot)))]))






(provide (all-defined-out))