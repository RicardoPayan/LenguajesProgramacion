#lang racket
(require racket/trace)

;unit-string?: string? -> boolean
(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))

;unit-string-list?: list? -> boolean
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

;explode: string? -> list
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

;implode: list? -> string
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

;Problema 3

;take: list?, integer? -> list
(define (take ls n)
  (cond
    [(or (equal? n 0) (empty? ls)) null]
    [else (cons (first ls) (take (rest ls) (sub1 n)))]
  ))


;drop: list?, integer? -> list
(define (drop ls n)
  (cond
    [(or (equal? n 0) (empty? ls)) ls]
    [else (drop (rest ls) (sub1 n))]
  ))


;bundle: (list-of-symbol?), integer? -> list
(define (bundle s n)
  (cond
    [(and (> n 0) (list? s))
    (cond
      [(or (null? s) (or (zero? n) (< n 0))) s]
      [else
       (cons (implode (take s n))
             (bundle (drop s n) n))])]
     [else (error 'bundle "n es menor que cero o no pasaste una lista")]))

;Problema 6

;list->chunks: list?, integer? -> list
(define (list->chunks ls n)
  (cond
    [(empty? ls) null]
    [else (cons (take ls n) (list->chunks (drop ls n) n))]))

;bundle-chunk: list?, integer? -> list
(define (bundle-chunk s n)
 (cond
   [(or (null? s) (or (zero? n) (< n 0))) s]
   [else (let add ([ls (list->chunks s n)])
           (cond
             [(empty? ls) null]
             [else (cons (implode (first ls)) (add (rest ls)))]))]))

;Problema 7

;partition: string?, integer? -> list
(define (partition s n)
  (cond
    [(or (null? s) (or (zero? n) (< n 0))) s]
    [else (bundle (explode s) n)]))


;Problema 8

;isort: list?, procedure? -> list
(define (isort ls predicado)
  (if (empty? ls)
      null
      (insert (first ls)
              (isort (rest ls) predicado) predicado)))
 
;;insert: integer?, list?, procedure? -> list
(define (insert n ls predicado)
  (cond
    [(empty? ls) (list n)]
    [(predicado n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) predicado))]))

;Problema 10-15

;same: list?, number? -> list
(define (same ls pivot)
  (cond
    [(empty? ls) null]
    [(filter (equal? (first ls) pivot) ls) (cons (first ls) (same (rest ls) pivot))]
    [else (same (rest ls) pivot)]))

;quicksort: list?, procedure? -> list
(define (quicksort ls predicado)
  (unless (procedure? predicado) (error 'quicksort "Esperaba un predicado valido, recibi ~e" predicado))
  (cond
    [(empty? ls) null]
    [(< (length ls) 100) (isort ls predicado)]
    [else
     (define pivot (first ls))
     (define smallers (filter (lambda (x) (predicado x pivot) ls)))
     (define largers (filter (lambda (x) (filter (and (not (predicado x pivot)) (not (equal? x pivot))) ) ls)))
     (append (quicksort (smallers ls pivot predicado) predicado) (same ls pivot) (quicksort (largers ls pivot predicado) predicado))]))






(provide (all-defined-out))