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
    [(equal? (first ls) primero) (cons (cons segundo (first ls)) (insertL primero segundo (rest ls))) ]
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
    [else (cons (first list) (remv-1st simbolo (rest list)) )]
    )
  

  )


(provide (all-defined-out))
