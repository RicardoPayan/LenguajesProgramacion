#lang racket

;; Escribe aquí tus soluciones

;;1
;(define (countdown number)
  ;(when (> number -1)
    ;(printf "~a\n" (append (list) (list number)))
    ;(countdown ( - number 1))
    ;))
(define (countdown number)
  (cond
    [(< number 0) null]
    [else (cons number (countdown (- number 1)))]))

 


(provide (all-defined-out))
