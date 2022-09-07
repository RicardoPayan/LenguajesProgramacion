#lang racket
(require pict
         pict/color)
(require racket/draw)

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


;Problema 21
(define (gcd-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

(define (gcd-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))


;el pilon
(define (triangle side width color)
  (define w side)
  (define h (* side (sin (/ pi 3))))
  (define (draw-it ctx dx dy)
    (define prev-pen (send ctx get-pen))
    (define path (new dc-path%))
    (send ctx set-pen (new pen% [width width] [color color]))
    (send path move-to 0 h)
    (send path line-to w h)
    (send path line-to (/ w 2) 0)
    (send path close)
    (send ctx draw-path path dx dy)
    (send ctx set-pen prev-pen))
  (dc draw-it w h))



(define (sierpinski side)
  (cond [(<= side 4) (triangle side 1 "red")]
        [else
         (define half (sierpinski (/ side 2)))
         (vc-append half (hc-append half half))]))

;Fractal de triangulos rectangulos invertidos, hechos de cuadrados.
;La verdad, salio mientras jugaba con pict.
;Me gusto porque la gran figura final es un triangulo rectangulo con una orientacion,
;pero los pequeños triangulos tienen la orientacion contraria
(define (triangleOfRectangles w h)
  (cond [(or (<= w 10) (<= h 10)) (filled-rectangle w h #:color "red")]
        [else
         (define half (triangleOfRectangles (/ w 2) (/ h 2)))
         (ht-append half (vc-append half half ))]))

(provide (all-defined-out))