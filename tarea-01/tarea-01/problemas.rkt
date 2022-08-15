#lang racket

;; Escribe aquí tus soluciones

;;1
(define (countdown number)
  (cond
    [(< number 0) null]
    [else (cons number (countdown (- number 1)))]))

;;2


(define (insertL primero segundo ls)

  
(cond
    [(empty? ls) null ]
    [(equal? (first ls) primero) (cons segundo (cons (first ls) (insertL primero segundo (rest ls))))]
    [else (cons (first ls) (insertL primero segundo (rest ls)))]))
   

  ;(cond
    ;[(empty? ls) null ]
    ;[(equal? (first ls) primero) (const segundo(cons (first ls) (cons (insertL primero segundo (rest ls)) null)))]
    ;[else (cons (first ls) (insertL primero segundo (rest ls)))]))
 

;;3
(define (remv-1st simbolo list)
  (cond
    [(empty? list) null]
    [(equal? (first list) simbolo) cons(rest list)]
    [else (cons (first list) (remv-1st simbolo (rest list)) )]))

;;4
(define (map p ls)
  (cond
    [(empty? ls) null]
    [else (cons (p (first ls)) (map p (rest ls)))]))

;;5
(define (filter p list)
  (cond
    [(empty? list) null]
    [(p (first list)) (cons (first list) (filter p (rest list)))]
    [else (filter p (rest list))]))

;;6
(define (zip ls1 ls2)
  (cond
    [(or (empty? ls1) (empty? ls2)) null]
    [else (cons (cons (first ls1) (first ls2)) (zip (rest ls1) (rest ls2)))]))

;;7
(define iter 0)
(define (list-index-ofv elemento list)
  
  (cond
    [(empty? list) null]
    [(equal? (first list) elemento) iter]
    [(+ iter 1)]
    [else(list-index-ofv elemento (rest list))]
    ))

;;8
(define (append ls1 ls2)

  (cond
    [(and (empty? ls1) (empty? ls2)) null]
    [(not (empty? ls1))  (cons (first ls1) (append (rest ls1) ls2))]
    [else (cons (first ls2) (append ls1 (rest ls2)))]
    ))

(provide (all-defined-out))
