#lang plait



(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))



;ArithC
(define-type ArithC
  [numC (n : Number)]
  [sumC (left : ArithC) (right : ArithC)]
  [mulC (left : ArithC) (right : ArithC)])

;ArithS
(define-type ArithS
  [numS (n : Number)]
  [sumS (left : ArithS) (right : ArithS)]
  [minuS (left : ArithS) (right : ArithS)]
  [mulS (left : ArithS) (right : ArithS)]
  [uminuS (e : ArithS)])


;Parse
(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (case (s-exp->symbol (first ls))
             [(+) (sumS (parse (second ls)) (parse (third ls)))]
             [(*) (mulS (parse (second ls)) (parse (third ls)))]
             [(-) (minuS (parse (second ls)) (parse (third ls)))]
             [else (error 'parse "operación aritmética malformada")]))]
        [else (error 'parse "expresión aritmética malformada")]))

;Interprete
(define (interp [a : ArithC]) : Number
  (type-case ArithC a
    [(numC n) n]
    [(sumC left right) (+ (interp left) (interp right))]
    [(mulC left right) (* (interp left) (interp right))]))

;Desugar
(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [(numS n) (numC n)]
    [(sumS left right) (sumC (desugar left) (desugar right))]
    [(mulS left right) (mulC (desugar left) (desugar right))]
    [(minuS left right) (sumC (desugar left) (mulC (numC -1) (desugar right)))]
    [(uminuS e) (mulC (numC -1) (desugar e))]))