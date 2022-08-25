#lang racket

;; Escribe aquí tus soluciones

;;1
(define (countdown number)
  (cond
    [(< number 0) null]
    [else (cons number (countdown (- number 1)))]))

;;2
(define (insertL primero segundo ls)
(cond
    [(empty? ls) null ]
    [(equal? (first ls) primero) (cons segundo (cons (first ls) (insertL primero segundo (rest ls))))]
    [else (cons (first ls) (insertL primero segundo (rest ls)))]))
  

;;3
(define (remv-1st simbolo list)
  (cond
    [(empty? list) null]
    [(equal? (first list) simbolo) cons(rest list)]
    [else (cons (first list) (remv-1st simbolo (rest list)) )]))

;;4
(define (map p ls)
  (cond
    [(empty? ls) null]
    [else (cons (p (first ls)) (map p (rest ls)))]))

;;5
(define (filter p list)
  (cond
    [(empty? list) null]
    [(p (first list)) (cons (first list) (filter p (rest list)))]
    [else (filter p (rest list))]))

;;6
(define (zip ls1 ls2)
  (cond
    [(or (empty? ls1) (empty? ls2)) null]
    [else (cons (cons (first ls1) (first ls2)) (zip (rest ls1) (rest ls2)))]))

;;7
(define (list-index-ofv elemento list)
  
  (cond
    [(empty? list) -1]
    [(equal? elemento (first list)) 0]
    [else (+ 1 (list-index-ofv elemento (rest list)))]
    ))

;;8
(define (append ls1 ls2)

  (cond
    [(and (empty? ls1) (empty? ls2)) null]
    [(not (empty? ls1))  (cons (first ls1) (append (rest ls1) ls2))]
    [else (cons (first ls2) (append ls1 (rest ls2)))]))

;;9
(define (reverse ls)
  (cond
    [(null? ls) null]
    [else  (append (reverse (rest ls)) (list (first ls)))]))

;;10
(define (repeat ls count)
  (cond
    [(equal? count 1) ls]
    [else (append (repeat ls (- count 1)) ls)]))

;;11
(define (same-lists* ls1 ls2)
  (cond 
  [(and (empty? ls1) (empty? ls2)) #t]
  [(or (empty? ls1) (empty? ls2)) #f]
  [(equal? (first ls1) (first ls2)) (same-lists* (rest ls1) (rest ls2))]
  [else #f]))

;;12
(define (equivalente)
  (equal? '((w . (x . ())) y (z . ())) '((w x) y (z))))

;;13
(define (binary->natural ls)
  (binary-helper ls 0))

(define (binary-helper ls n)
  (cond
    [(empty? ls) 0]
    [(equal? (first ls) 1) (+ (expt 2 n) (binary-helper (rest ls) (add1 n)))]
    [else (binary-helper (rest ls) (add1 n))]))

;;14
(define (div a b)
  (div-helper a b 1)
  )

(define (div-helper a b n)

  (cond
    [(< a b) #f]
    [(> (* b n) a) #f]
    [(equal? (* b n) a) n]
    [else (div-helper a b (add1 n))]))


;;15
(define (append-map p arg)
  (cond
    [(null? arg) null]
    [else (append (p (first arg)) (append-map p (rest arg)))]))

;;16
(define (set-difference s1 s2)
  (cond
    [(empty? s1) null]
    [(equal? (member (first s1) s2) #f) (cons (first s1) (set-difference (rest s1) s2))]
    [else (set-difference (rest s1) s2)]))

;;17
(define (foldr op n ls)
  (cond
   [(empty? ls) n]
   [else (op (first ls) (foldr op n (rest ls)))]))


;;18
(define (powerset ls)
  (cond
    [(empty? ls) (list ls)]
    [else (let ([ps (powerset (rest ls))]) (append (f (first ls) ps) ps))]))

(define (f x ls)
  (cond
    [(null? ls) null]
    [else (cons (cons x (first ls)) (f x (rest ls)))]
  ))

;19
(define (cartesian-product ls)
  (cond
    [(empty? (first ls)) null]
    [else (append (cartesian-helper (first (first ls)) (second ls)) (cartesian-product (list (rest (first ls)) (second ls)) ))]
    ))

(define (cartesian-helper number lis)

  (cond
    [(empty? lis) null]
    [else (cons (cons number (list (first lis))) (cartesian-helper number (rest lis)))]))

;20
;insertL
(define (insertL-fr primero segundo ls )
  (foldr  (lambda (primero* ls*)
            (cond
              [(equal? primero primero*) (cons segundo (cons primero* ls*))]
              [else (cons primero* ls*)]))
          '() ls ))

;filter-fr
(define (filter-fr predicado ls)
  (foldr (lambda (primero ls*)
           (cond
           [(predicado primero) (cons primero ls*)]
           [else ls*])
           ) '() ls))

;map-fr
(define (map-fr procedimiento ls)
  (foldr (lambda (primero ls*)
           [cons (procedimiento primero) ls*]
           )'() ls))

(define (map2 procedimiento ls)
  (foldr map (map procedimiento ls) '()))

;append-fr
(define (append-fr ls1 ls2)
  (foldr append (append ls1 ls2) '())
  )

;reverse-fr
(define (reverse-fr ls)
  (foldr reverse (reverse ls) '()))

;binary->natural-fr
(define (binary->natural-fr ls)
  (foldr binary->natural (binary->natural ls) '()))

;append-map-fr
(define (append-map-fr p arg)
  (foldr append-map (append-map p arg) '()))

;set-difference-fr
(define (set-difference-fr s1 s2)
  (foldr set-difference (set-difference s1 s2) '()))

;powerset-fr
(define (powerset-fr ls)
  (foldr powerset (powerset ls) '()))

;21
(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (odd? x))
               (snowball (add1 (* x 3))))
              (else (fix-odd x))))))
       (even-case
        (lambda (fix-even)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (even? x))
               (snowball (/ x 2)))
              (else (fix-even x))))))
       (one-case
        (lambda (fix-one)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (fix-one x))))))
       (base
        (lambda (x)
          (error 'error "Invalid value ~s~n" x))))
    (one-case(even-case(odd-case base)))))



(provide (all-defined-out))