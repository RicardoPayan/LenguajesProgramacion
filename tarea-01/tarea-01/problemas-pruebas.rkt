#lang racket

(require rackunit
         rackunit/text-ui
         "problemas.rkt")

(define-test-suite pruebas
  (test-case "countdown"
    (check-equal? (countdown 5)
                  '(5 4 3 2 1 0)))
  
  (test-case "insertL"
    (check-equal? (insertL 'x 'y '(x z z x y x))
                  '(y x z z y x y y x)))
  
  (test-case "remv-1st"
    (check-equal? (remv-1st 'x '(x y z x))
                  '(y z x))
    (check-equal? (remv-1st 'y '(x y z y x))
                  '(x z y x))
    (check-equal? (remv-1st 'z '(a b c))
                  '(a b c)))
  
  (test-case "map"
    (check-equal? (map sub1 '(1 2 3 4))
                  '(0 1 2 3)))
  
  (test-case "filter"
    (check-equal? (filter even? '(1 2 3 4 5 6))
                  '(2 4 6)))
  
  (test-case "zip"
    (check-equal? (zip '(1 2 3) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3 4 5 6) '(a b c))
                  '((1 . a) (2 . b) (3 . c)))
    (check-equal? (zip '(1 2 3) '(a b c d e f))
                  '((1 . a) (2 . b) (3 . c))))
  
  (test-case "list-index-ofv"
    (check-eqv? (list-index-ofv 'x '(x y z x x)) 0)
    (check-eqv? (list-index-ofv 'x '(y z x x)) 2))
  
  (test-case "append"
    (check-equal? (append '(42 120) '(1 2 3))
                  '(42 120 1 2 3))
    (check-equal? (append '(a b c) '(cat dog))
                  '(a b c cat dog)))
  
  (test-case "reverse"
    (check-equal? (reverse '(a 3 x))
                  '(x 3 a)))
  
  (test-case "repeat"
    (check-equal? (repeat '(4 8 11) 4)
                  '(4 8 11 4 8 11 4 8 11 4 8 11)))
  
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
  
  (test-case "foldr"
    (check-equal? (foldr cons '() '(1 2 3 4))
                  '(1 2 3 4))
    (check-eqv? (foldr + 0 '(1 2 3 4))
                10)
    (check-eqv? (foldr * 1 '(1 2 3 4))
                24))
  
  (test-case "powerset"
    (check-equal? (powerset '(3 2 1))
                  '((3 2 1) (3 2) (3 1) (3) (2 1) (2) (1) ()))
    (check-equal? (powerset '())
                  '(())))
  
  (test-case "cartesian-product"
    (check-equal? (cartesian-product '((5 4) (3 2 1)))
                  '((5 3) (5 2) (5 1) (4 3) (4 2) (4 1))))
  
  (test-case "snowball"
    (check-eqv? (snowball 12) 1)
    (check-eqv? (snowball 120) 1)
    (check-eqv? (snowball 9999) 1))
  
  (test-case "snowball"
    (let ((ns (make-base-namespace)))
      (check-equal? (eval quine ns) quine)
      (check-equal? (eval (eval quine ns) ns) quine))))

(run-tests pruebas 'verbose)
