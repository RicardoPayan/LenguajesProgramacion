#lang racket

;; 1
(define pi 3.14)

;; 2.
(define (area-circle r)
  (* pi  (* r r)))

;;3
(define (circle-properties r)
  (list (area-circle r) (* 2 pi r)))

;;4
(define (rectangle-properties rec)
  (define l (list-ref rec 0))
  (define a (list-ref rec 1))

  (list(* l a) (+ l l a a)) )

;;5
(define (find-needle ls)
 (cond
    [(empty? ls) 0]
    [(equal? (first ls) 'needle) 1]
    [else (+ 1 (find-needle(rest ls)))]))

;; 6.
(define (abs x)
(cond
  [(< x 0) (* x -1)]
  [else x]
  ))

;; 7.
(define (inclis1 ls)
  (map (lambda (i) (+ 1 i)) ls)
)

;; 8.
;;(define (even? x)
;;  ...)

;; 9.
(define another-add
  (lambda (n m)
    (cond
      [(zero? n) m]
      [else (add1 (another-add m (sub1 n)))]
      )))



(provide (all-defined-out))
