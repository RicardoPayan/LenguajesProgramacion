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
             (check-exn exn:fail? (thunk (bundle '("") 3))))


  (test-case "partition vs bundle"
             (equal? (partition "abcd" 2) (bundle (explode "abcd") 2)))
  
  (test-case "isort"
             (equal? (isort '(#\z #\c #\b #\a #\e) char<=?) (list #\a #\b #\c #\e #\z))
             (equal? (isort '(3 4 1 7 34) <) '(1 3 4 7 34))
             (equal? (isort '("z" "Apple" "queen" "sexy") string<=?) (list "Apple" "queen" "sexy" "z"))
             (equal? (isort '(#"b" #"a" #"c") bytes<?) (list #"a" #"b" #"c"))
             (equal? (isort (list 'b 'a 'c) symbol<?) (list 'a 'b 'c))
             (equal? (isort (list '#:b '#:a '#:c) keyword<?) (list '#:a '#:b '#:c)))
  
  (test-case "quicksort"
             (equal?  (quicksort '("z" "Apple" "queen" "sexy") string<?) (list "Apple" "queen" "sexy" "z") )
             (equal? (quicksort (list 'b 'a 'c) symbol<?) (list 'a 'b 'c))
             (equal? (quicksort '(1 2 3 5 2 5 7 9) >) '(9 7 5 5 3 2 2 1))
             (equal? (quicksort '(1 2 3 5 2 5 7 9) <) '(1 2 2 3 5 5 7 9)))
  )

(run-tests pruebas 'verbose)