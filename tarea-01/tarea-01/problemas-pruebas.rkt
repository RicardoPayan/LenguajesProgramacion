#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  
  (test-case "countdown"
    (check-equal? (countdown 5)
                  '(5 4 3 2 1 0))
    (check-equal? (countdown 0)
                  '(0)))
  
  (test-case "insertL"
    (check-equal? (insertL 'x 'y '(x z z x y x))
                  '(y x z z y x y y x))
    (check-equal? (insertL 'x 'y '())
                  '()))
  
  (test-case "remv-1st"
    (check-equal? (remv-1st 'x '(x y z x))
                  '(y z x))
    (check-equal? (remv-1st 'y '(x y z y x))
                  '(x z y x))
    (check-equal? (remv-1st 'z '(a b c))
                  '(a b c))
    (check-equal? (remv-1st 'z '())
                  '()))
  
  (test-case "map"
    (check-equal? (map sub1 '(1 2 3 4))
                  '(0 1 2 3))
    (check-equal? (map abs '(-1 -2 -3 4))
                  '(1 2 3 4))
    (check-equal? (map add1 '(1 2 3 4))
                  '(2 3 4 5))
    (check-equal? (map sub1 '())
                  '()))
  
  (test-case "filter"
    (check-equal? (filter even? '(1 2 3 4 5 6))
                  '(2 4 6))
    (check-equal? (filter number? '(1 "hello" 3 "yes"))
                  '(1 3))
    (check-equal? (filter real? '(1 2+3i 4))
                  '(1 4))
    (check-equal? (filter even? '())
                  '()))
  
  (test-case "zip"
    (check-equal? (zip '(1 2 3) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3 4 5 6) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3) '(a b c d e f))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '() '())
                  '())
    (check-equal? (zip '(1 2 3) '())
                  '()))
  
  (test-case "list-index-ofv"
    (check-eqv? (list-index-ofv 'x '(x y z x x)) 0)
    (check-eqv? (list-index-ofv 'x '(y z x x)) 2)
    (check-eqv? (list-index-ofv 'f '(y z x x)) 3)) ;Si no esta el termino, se devuelve la ultima posicion de la lista.
  
  (test-case "append"
    (check-equal? (append '(42 120) '(1 2 3))
                  '(42 120 1 2 3))
    (check-equal? (append '(a b c) '(cat dog))
                  '(a b c cat dog))
    (check-equal? (append '() '(cat dog))
                  '(cat dog))
    (check-equal? (append '() '())
                  '()))
  
  (test-case "reverse"
    (check-equal? (reverse '(a 3 x))
                  '(x 3 a))
    (check-equal? (reverse '())
                  '()))
  
  (test-case "repeat"
    (check-equal? (repeat '(4 8 11) 4)
                  '(4 8 11 4 8 11 4 8 11 4 8 11))
    (check-equal? (repeat '() 4)
                  '()))
  
  (test-case "same-lists*"
    (check-true (same-lists* '() '()))
    (check-true (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
    (check-false (same-lists* '(1 2 3 4) '(1 2 3 4 5)))
    (check-false (same-lists* '(a (b c) d) '(a (b) c d)))
    (check-true (same-lists* '((a) b (c d) d) '((a) b (c d) d))))
  
  
  (test-case "binary->natural"
             (check-eqv? (binary->natural '()) 0)
             (check-eqv? (binary->natural '(0 0 1)) 4)
             (check-eqv? (binary->natural '(0 0 1 1)) 12)
             (check-eqv? (binary->natural '(1 1 1 1)) 15)
             (check-eqv? (binary->natural '(1 0 1 0 1)) 21)
             (check-eqv? (binary->natural '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))
  
  (test-case "div"
             (check-eqv? (div 25 5) 5)
             (check-eqv? (div 36 6) 6))
  
  (test-case "append-map"
             (check-equal? (append-map countdown (countdown 5))
              '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)))

  (test-case "set-difference"
             (check-equal? (set-difference '(1 2 3 4 5) '(2 6 4 8))
                           '(1 3 5)))
  (test-case "powerset"
             (check-equal? (powerset '(3 2 1))
                           '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
             (check-equal? (powerset '())
                           '(())))
  (test-case "foldr"
             (check-equal? (foldr cons '() '(1 2 3 4))
                           '(1 2 3 4))
             (check-eqv? (foldr + 0 '(1 2 3 4))
                         10)
             (check-eqv? (foldr * 1 '(1 2 3 4))
                         24))
  (test-case "cartesian-product"
             (check-equal? (cartesian-product '((5 4) (3 2 1)))
                           '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
  
  (test-case "insertL-fr"
             (check-equal? (insertL-fr 'x 'y '(x z z x y x))
                           '(y x z z y x y y x)))

  (test-case "filter-fr"
    (check-equal? (filter-fr even? '(1 2 3 4 5 6))
                  '(2 4 6)))

  (test-case "map-fr"
    (check-equal? (map2 sub1 '(1 2 3 4))
                  '(0 1 2 3)))

  (test-case "append-fr"
    (check-equal? (append-fr '(42 120) '(1 2 3))
                  '(42 120 1 2 3))
    (check-equal? (append-fr '(a b c) '(cat dog))
                  '(a b c cat dog)))

  (test-case "reverse-fr"
    (check-equal? (reverse-fr '(a 3 x))
                  '(x 3 a)))

  (test-case "binary->natural-fr"
             (check-eqv? (binary->natural-fr '()) 0)
             (check-eqv? (binary->natural-fr '(0 0 1)) 4)
             (check-eqv? (binary->natural-fr '(0 0 1 1)) 12)
             (check-eqv? (binary->natural-fr '(1 1 1 1)) 15)
             (check-eqv? (binary->natural-fr '(1 0 1 0 1)) 21)
             (check-eqv? (binary->natural-fr '(1 1 1 1 1 1 1 1 1 1 1 1 1)) 8191))

  (test-case "append-map-fr"
             (check-equal? (append-map-fr countdown (countdown 5))
              '(5 4 3 2 1 0 4 3 2 1 0 3 2 1 0 2 1 0 1 0 0)))

  (test-case "set-difference-fr"
             (check-equal? (set-difference-fr '(1 2 3 4 5) '(2 6 4 8))
                           '(1 3 5)))

  (test-case "powerset-fr"
             (check-equal? (powerset-fr '(3 2 1))
                           '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
             (check-equal? (powerset '())
                           '(())))
  (test-case "snowball"
             (check-eqv? (snowball 12) 1)
             (check-eqv? (snowball 120) 1)
             (check-eqv? (snowball 9999) 1))

)

(run-tests pruebas 'verbose)
