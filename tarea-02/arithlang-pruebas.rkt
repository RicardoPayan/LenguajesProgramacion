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

;Negativo
(test (eval `(- 1)) -1)

;Errores
(test/exn (eval `()) "parse: operación aritmética malformada")
(test/exn (eval `(1 * 1)) "parse: No es expresion valida")
(test/exn (eval `(/ 1 1)) "parse: operación aritmética invalida")