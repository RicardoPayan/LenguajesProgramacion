#lang plait
(require "arithlang.rkt")

;Numero
(test (eval `5) 5)
(test (eval `5.1) 5.1)

;Suma
(test (eval `(+ 1 1)) 2)

;Producto
(test (eval `(* 1 2)) 2)
(test (eval `(* -1 2)) -2)
(test (eval `(* 0.345 3)) 1.035)

;Resta
(test (eval `(- 1 1)) 0)


