#lang plait

(require "interprete.rkt")

(print-only-errors #t)

;;************************************************;;
;; Calando expresiones que se evalúan a si mismas ;;
;;************************************************;;

;; numeros
(test (eval `0) (numV 0))

(test (eval `5) (numV 5))

;; cadenas
(test (eval `"") (strV ""))

(test (eval `"cadena") (strV "cadena"))

;; booleanos
(test (eval `false) (boolV #f))

(test (eval `true) (boolV #t))

;;******************************;;
;; Calando operaciones binarias ;;
;;******************************;;

;; suma
(test (eval `(+ 0 0)) (numV 0))

(test (eval `(+ 0 1)) (numV 1))

(test (eval `(+ 1 0)) (numV 1))

(test (eval `(+ 1 1)) (numV 2))

(test (eval `(+ 42 624)) (numV 666))

(test (eval `(+ (+ 1 2) 3)) (numV 6))

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

;; equivalencia entre cadenas
(test (eval `(str= "" "")) (boolV #t))

(test (eval `(str= "hola" "hola")) (boolV #t))

(test (eval `(str= "hola" "adios")) (boolV #f))

(test (eval `(str= (++ "foo" "bar")
                   (++ (++ "f" "oo") (++ "ba" "r"))))
      (boolV #t))

;; condiciones de error
(test/exn (eval `(+ 0 "")) "argumento incorrecto")

(test/exn (eval `(++ 1 2)) "argumento incorrecto")

;;******************************;;
;; Calando operadores booleanos ;;
;;******************************;;

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

;;**************************;;
;; Calando condicionales if ;;
;;**************************;;

(test (eval `(if true 0 "")) (numV 0))

(test (eval `(if false 0 "")) (strV ""))

(test (eval `(++ "que "
                 (if (and (num= (+ 666 42) 708)
                          (str= "Alan Turing" (++ "Alan" "Turing")))
                     "bien"
                     "mal")))
      (strV "que mal"))

;;*******************;;
;; Calando funciones ;;
;;*******************;;

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

;;******************************;;
;; Calando definiciones locales ;;
;;******************************;;

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

(test (eval `(let (f (fun x (+ x 666)))
               (let (g (fun y (+ (f 123) y)))
                 (g (f 1)))))
      (numV 1456))

;; condiciones de error
(test/exn (eval `(let (x (+ y 2))
                   (+ x x)))
          "identificador no está enlazado")