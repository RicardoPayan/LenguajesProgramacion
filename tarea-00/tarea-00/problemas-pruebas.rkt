#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "area-circle"
    (check-eqv? (area-circle 5) 78.5))
  
  (test-case "circle-properties"
    (check-within (circle-properties 5) '(78.5 31.4) 0.001))
  
  (test-case "rectangle-properties"
    (check-equal? (rectangle-properties '(2 4)) '(8 12)))
  
  (test-case "find-needle"
    (check-eqv? (find-needle '(hay needle hay)) 1)
    (check-eqv? (find-needle '(hay hay hay)) -1))
  
  (test-case "abs"
    (check-eqv? (abs 3) 3)
    (check-eqv? (abs -2) 2))
  
  (test-case "inclis1"
    (check-equal? (inclis1 '(1 2 3)) '(2 3 4)))
  
  (test-case "even?"
    (check-equal? (map even? '(1 2 3 4 5 6)) '(#f #t #f #t #f #t)))
  
  (test-case "another-add"
    (check-eqv? (another-add 10 5) 15)))

(run-tests pruebas 'verbose)
