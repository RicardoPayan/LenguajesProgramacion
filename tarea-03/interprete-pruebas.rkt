#lang plait

(require "interprete.rkt")

(print-only-errors #t)

;;*************************************************;;
;; Checando expresiones que se evalúan a si mismas ;;
;;*************************************************;;

;; numeros
(test (eval `0) (numV 0))

(test (eval `5) (numV 5))

(test (eval `2/3) (numV 2/3))

(test (eval `-10.0) (numV -10.0))

(test (eval `#b101010) (numV 42))

(test (eval `4-3.1i) (numV 4-3.1i))

;; cadenas
(test (eval `"") (strV ""))

(test (eval `"cadena") (strV "cadena"))

(test (eval `"LA CADENA") (strV "LA CADENA"))

;; booleanos
(test (eval `false) (boolV #f))

(test (eval `true) (boolV #t))

(test/exn (eval `#false) "expresión malformada")

(test/exn (eval `#true) "expresión malformada")

(test/exn (eval `#f) "expresión malformada")

(test/exn (eval `#t) "expresión malformada")

;;*******************************;;
;; Checando operaciones binarias ;;
;;*******************************;;

;; suma
(test (eval `(+ 0 0)) (numV 0))

(test (eval `(+ 0 1)) (numV 1))

(test (eval `(+ 1 0)) (numV 1))

(test (eval `(+ 1 1)) (numV 2))

(test (eval `(+ 42 624)) (numV 666))

(test (eval `(+ (+ 1 2) 3)) (numV 6))

(test (eval `(+ 0+i 1-i)) (numV 1))

(test (eval `(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 (+ 10 11)))))))))))
      (numV 66))

(test (eval `(+ (+ (+ 1 (+ 2 3))
                   (+ (+ 4 5) 6))
                (+ (+ 7 (+ 8 9))
                   (+ (+ 10 11) 12))))
      (numV 78))

;; concatenación
(test (eval `(++ "" "")) (strV ""))

(test (eval `(++ "x" "")) (strV "x"))

(test (eval `(++ "" "x")) (strV "x"))

(test (eval `(++ "abc" "def")) (strV "abcdef"))

(test (eval `(++ "a" (++ "b" (++ "c" (++ "d" (++ "e" "f"))))))
      (strV "abcdef"))

(test (eval `(++ (++ "a" (++ "b" "c"))
                 (++ (++ "d" "e") "f")))
      (strV "abcdef"))

;; equivalencia entre números
(test (eval `(num= 0 0)) (boolV #t))

(test (eval `(num= 0 1)) (boolV #f))

(test (eval `(num= (+ 0 1) (+ 1 2))) (boolV #f))

(test (eval `(num= (+ 1 (+ 2 3)) (+ (+ 3 2) 1))) (boolV #t))

(test (eval `(num= 0.0 0)) (boolV #t))

(test (eval `(num= -0.0 0)) (boolV #t))

(test (eval `(num= 0.1 1/10)) (boolV #f))

;; equivalencia entre cadenas
(test (eval `(str= "" "")) (boolV #t))

(test (eval `(str= "hola" "hola")) (boolV #t))

(test (eval `(str= "hola" "adios")) (boolV #f))

(test (eval `(str= (++ "foo" "bar")
                   (++ (++ "f" "oo") (++ "ba" "r"))))
      (boolV #t))

(test (eval `(str= "CADENA" "cadena")) (boolV #f))

;; condiciones de error
(test/exn (eval `(+ 0 "")) "argumento incorrecto")

(test/exn (eval `(++ 1 2)) "argumento incorrecto")

;;*******************************;;
;; Checando operadores booleanos ;;
;;*******************************;;

;; conjunción
(test (eval `(and false false)) (boolV #f))

(test (eval `(and false true)) (boolV #f))

(test (eval `(and true false)) (boolV #f))

(test (eval `(and true true)) (boolV #t))

(test (eval `(and (str= "" "") (num= 1 2))) (boolV #f))

;; disyunción
(test (eval `(or false false)) (boolV #f))

(test (eval `(or false true)) (boolV #t))

(test (eval `(or true false)) (boolV #t))

(test (eval `(or true true)) (boolV #t))

(test (eval `(or (str= "" "") (num= 1 2))) (boolV #t))

;; mezcla
(test (eval `(or (and true false) (and true true))) (boolV #t))

(test (eval `(and (or true false) (or false true))) (boolV #t))

(test (eval `(or (or (or false true) false) false)) (boolV #t))

(test (eval `(and (and (and true false) true) true)) (boolV #f))

;; condiciones de error
(test/exn (eval `(or 1 2)) "no es un valor booleano")

(test/exn (eval `(or false 2)) "no es un valor booleano")

(test (eval `(or true 2)) (boolV #t))

(test/exn (eval `(and "foo" "bar")) "no es un valor booleano")

(test/exn (eval `(and true "bar")) "no es un valor booleano")

(test (eval `(and false "bar")) (boolV #f))

;;***************************;;
;; Checando condicionales if ;;
;;***************************;;

(test (eval `(if true 0 "")) (numV 0))

(test (eval `(if false 0 "")) (strV ""))

(test (eval `(++ "que "
                 (if (and (num= (+ 666 42) 708)
                          (str= "Alan Turing" (++ "Alan" "Turing")))
                     "bien"
                     "mal")))
      (strV "que mal"))

;;********************;;
;; Checando funciones ;;
;;********************;;

(test (eval `((fun x (+ x x)) 5)) (numV 10))

(test (eval `((fun x (if (num= x 0) "foo" 666)) 0)) (strV "foo"))

(test (eval `((fun x (if (num= x 0) "foo" 666)) 1)) (numV 666))

(test (eval `((if (str= "foo" "bar")
                  (fun x x)
                  (fun y (++ y "!")))
              "felicidades"))
      (strV "felicidades!"))

;; condiciones de error
(test/exn (eval `((+ 1 2) 3)) "no es una función")

(test/exn (eval `((fun x (x 1)) (fun y (++ y y)))) "argumento incorrecto")

;;*******************************;;
;; Checando definiciones locales ;;
;;*******************************;;

(test (eval `(let (x 1) x)) (numV 1))

(test (eval `(let (x 1)
               (let (y 2)
                 (let (z 3)
                   (+ z (+ y x))))))
      (numV 6))

(test (eval `(let (x 1)
               (+ x (let (y (+ x x))
                      (+ y (let (z (+ x y)) z))))))
      (numV 6))

(test (eval `(let (w 28)
               (+ (let (x 666) (+ x w))
                  (let (y 69) (+ w y)))))
      (numV 791))

(test (eval `(let (x 1)
               (+ x (let (x 2)
                      (+ x (let (x 3)
                             (+ x x)))))))
      (numV 9))

(test (eval `(let (x 1)
               (+ (let (x 2)
                    (+ (let (x 3)
                         (+ x x))
                       x))
                  x)))
      (numV 9))

(test (eval `(let (f (fun x (+ x 666)))
               (let (g (fun y (+ (f 123) y)))
                 (g (f 1)))))
      (numV 1456))

(test (eval `(let (Y (fun f ((fun x (f (fun y ((x x) y))))
                             (fun x (f (fun y ((x x) y)))))))
               (let (f (fun f (fun x (if (num= x 0)
                                         0
                                         (+ x (f (+ x -1)))))))
                 (let (sum-upto (Y f))
                   (sum-upto 3)))))
      (numV 6))

(test (eval `(let (Y (fun f ((fun x (f (fun y ((x x) y))))
                             (fun x (f (fun y ((x x) y)))))))
               (let (f (fun f (fun x (if (num= x 0)
                                         0
                                         (+ x (f (+ x -1)))))))
                 (let (sum-upto (Y f))
                   (sum-upto 10)))))
      (numV 55))

(test (eval `(let (Y (fun f ((fun x (f (fun y ((x x) y))))
                             (fun x (f (fun y ((x x) y)))))))
               (let (empty (fun op (if (num= op 2) true false)))
                 (let (pair (fun first (fun rest (fun op (if (num= op 0)
                                                             first
                                                             (if (num= op 1)
                                                                 rest
                                                                 false))))))
                   (let (first (fun pair (pair 0)))
                     (let (rest (fun pair (pair 1)))
                       (let (empty? (fun pair (pair 2)))
                         (let (f (fun f (fun list (if (empty? list)
                                                      0
                                                      (+ 1 (f (rest list)))))))
                           (let (length (Y f))
                             (let (mandado
                                   ((pair "Leche")
                                    ((pair "Huevos")
                                     ((pair "Tortillas")
                                      ((pair "Frijoles")
                                       ((pair "Arroz")
                                        empty))))))
                               (length mandado)))))))))))
      (numV 5))

;; condiciones de error
(test/exn (eval `(let (x (+ y 2))
                   (+ x x)))
          "identificador no está enlazado")

(test/exn (eval `(let (f (fun x (if (num= x 0) 0 (+ 1 (f (+ -1 x))))))
                   (f 3)))
          "identificador no está enlazado")