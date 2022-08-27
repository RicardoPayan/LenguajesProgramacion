#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")


(define-test-suite pruebas

  (test-case "Pasar lista con explode"
             (check-equal? (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g")))

  (test-case "Lista mas chica que el tamano de trozo"
             (check-equal? (bundle '("a" "b") 3)
                           (list "ab")))
  
  (test-case "Lista vacia"
             (check-equal? (bundle '() 3)
                           '()))
  (test-case "Trozo igual a cero"
             (check-equal? (bundle '("a" "b") 0)
                           (list "a" "b"))) ;No deberia modificar la lista
  
  (test-case "Lista vacia y trozo igual a cero"
             (check-equal? (bundle '() 0)
                           '()))
  (test-case "Trozo negativo"
             (check-equal? (bundle '("a" "b") -1)
                           (list "a" "b"))) ;No deberia modificar la lista
  (test-case "String vacio"
             (check-exn exn:fail? (thunk (bundle '("") 3))) 
))

(run-tests pruebas 'verbose)