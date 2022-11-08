;;; -*- mode: racket; coding: utf-8 -*-
;;; 
;;; `7MM"""Mq.  `7MM"""YMM    .g8"""bgd  
;;;   MM   `MM.   MM    `7  .dP'     `M  
;;;   MM   ,M9    MM   d    dM'       `  
;;;   MMmmdM9     MMmmMM    MM           
;;;   MM  YM.     MM   Y  , MM.    `7MMF'
;;;   MM   `Mb.   MM     ,M `Mb.     MM  
;;; .JMML. .JMM..JMMmmmmMMM   `"bmmmdPY
;;;
;;;       ~ Smart Constructors ~

#lang racket/base

(require racket/set
         racket/list
         "ast.rkt")

(provide nothing nothing?
         just-epsilon just-epsilon?
         just-char just-char? just-char-value)

;;; A finite set of chars

;;; char-set : char-like? ... -> char-set?
(define (char-set . xs)
  (let ([soc (set-of-chars 'char-set xs)])
    (if (zero? (set-count soc))
        (nothing)
        (make-char-set
         (lambda (x)
           (and (char? x)
                (set-member? soc x)))))))

;;; char-set-comp : char-like? ... -> char-set?
(define (char-set-comp . xs)
  (let ([soc (set-of-chars 'char-set-comp xs)])
    (make-char-set
     (lambda (x)
       (and (char? x)
            (not (set-member? soc x)))))))

;;; set-of-chars : listof(char-like?) -> setof(char?)
(define (set-of-chars who xs)
  (define (adjoin-char x soc)
    (cond [(string? x)
           (set-union soc (list->seteqv (string->list x)))]
          [(coerce-char x)
           => (lambda (c) (set-add soc c))]
          [else
           (error who "not a char-like value: ~v" x)]))
  (foldl adjoin-char (seteqv) xs))

;;; char-range : char-like? × char-like? -> char-set?
(define (char-range from upto)
  (define beg (coerce-char from))
  (define end (coerce-char upto))
  (unless beg
    (raise-argument-error 'char-range "char-like?" 0 from upto))
  (unless end
    (raise-argument-error 'char-range "char-like?" 1 from upto))
  (cond [(char<? end beg) (nothing)]
        [(char=? beg end) (just-char beg)]
        [else (make-char-set (lambda (c) (char<=? beg c end)))]))

(define just-alphabetic (make-char-set char-alphabetic?))
(define just-lower-case (make-char-set char-lower-case?))
(define just-upper-case (make-char-set char-upper-case?))
(define just-title-case (make-char-set char-title-case?))
(define just-numeric (make-char-set char-numeric?))
(define just-symbolic (make-char-set char-symbolic?))
(define just-punctuation (make-char-set char-punctuation?))
(define just-graphic (make-char-set char-graphic?))
(define just-whitespace (make-char-set char-whitespace?))
(define just-blank (make-char-set char-blank?))
(define just-iso-control (make-char-set char-iso-control?))

(provide char-set char-set? char-set-predicate
         char-set-comp
         char-range
         just-alphabetic
         just-lower-case
         just-upper-case
         just-title-case
         just-numeric
         just-symbolic
         just-punctuation
         just-graphic
         just-whitespace
         just-blank
         just-iso-control)

;;; Kleene closure of the given set

;;; reg-kleene : kindof-regular? -> regular?
(define (reg-kleene rset)
  (unless (kindof-regular? rset)
    (raise-argument-error 'reg-kleene "kindof-regular?" rset))
  (cond [(reg-kleene? rset) rset]
        [(nothing? rset) (just-epsilon)]
        [(just-epsilon? rset) rset]
        [else
         (let ([rset (ensure-regular 'reg-kleene rset)])
           (make-reg-kleene rset))]))

(provide reg-kleene reg-kleene? reg-kleene-set)

;;; Set complement of the given set

;;; reg-comp : kindof-regular? -> regular?
(define (reg-comp rset)
  (unless (kindof-regular? rset)
    (raise-argument-error 'reg-comp "kindof-regular?" rset))
  (cond [(reg-comp? rset) (reg-comp-set rset)]
        [else
         (let ([rset (ensure-regular 'reg-comp rset)])
           (make-reg-comp rset))]))

(provide reg-comp reg-comp? reg-comp-set)

;;; Union of left and right sets

;;; reg-union : kindof-regular? ... -> regular?
(define (reg-union . rsets)
  (define (union-binop left right)
    (unless (kindof-regular? left)
      (raise-argument-error 'reg-union "kindof-regular?" left))
    (unless (kindof-regular? right)
      (raise-argument-error 'reg-union "kindof-regular?" right))
    (cond [(reg-union? left)
           (union-binop (reg-union-left left)
                        (union-binop (reg-union-right left)
                                     right))]
          [(nothing? left) right]
          [(nothing? right) left]
          [(everything? left) left]
          [(everything? right) right]
          [(equal? left right) left]
          [else (make-reg-union left right)]))
  (foldr union-binop (nothing)
         (map (lambda (rset) (ensure-regular 'reg-union rset)) rsets)))

(provide reg-union reg-union? reg-union-left reg-union-right)

;;; Intersection of left and right sets

;;; reg-inter : kindof-regular? ... -> regular?
(define (reg-inter . rsets)
  (define (inter-binop left right)
    (unless (kindof-regular? left)
      (raise-argument-error 'reg-inter "kindof-regular?" left))
    (unless (kindof-regular? right)
      (raise-argument-error 'reg-inter "kindof-regular?" right))
    (cond [(reg-inter? left)
           (inter-binop (reg-inter-left left)
                        (inter-binop (reg-inter-right left)
                                     right))]
          [(nothing? left) left]
          [(nothing? right) right]
          [(everything? left) right]
          [(everything? right) left]
          [(equal? left right) left]
          [else (make-reg-inter left right)]))
  (foldr inter-binop (everything)
         (map (lambda (rset) (ensure-regular 'reg-inter rset)) rsets)))

(provide reg-inter reg-inter? reg-inter-left reg-inter-right)

;;; Concatenation of left and right sets

;;; reg-conc : kindof-regular? ... -> regular?
(define (reg-conc . rsets)
  (define (conc-binop left right)
    (unless (kindof-regular? left)
      (raise-argument-error 'reg-conc "kindof-regular?" left))
    (unless (kindof-regular? right)
      (raise-argument-error 'reg-conc "kindof-regular?" right))
    (cond [(reg-conc? left)
           (conc-binop (reg-conc-left left)
                       (conc-binop (reg-conc-right left)
                                   right))]
          [(nothing? left) left]
          [(nothing? right) right]
          [(just-epsilon? left) right]
          [(just-epsilon? right) left]
          [else (make-reg-conc left right)]))
  (foldr conc-binop (just-epsilon)
         (map (lambda (rset) (ensure-regular 'reg-conc rset)) rsets)))

(provide reg-conc reg-conc? reg-conc-left reg-conc-right)

;;; just-string : string? -> regular?
(define (just-string s)
  (unless (string? s)
    (raise-argument-error 'just-string "string?" s))
  (apply reg-conc (string->list s)))

;;; reg-maybe : kindof-regular? -> regular?
(define (reg-maybe rset)
  (unless (kindof-regular? rset)
    (raise-argument-error 'reg-maybe "kindof-regular?" rset))
  (reg-union rset (just-epsilon)))

;;; reg-repeat : integer? × integer? × kindof-regular? -> regular?
(define (reg-repeat lo hi rset)
  (unless (exact-nonnegative-integer? lo)
    (raise-argument-error 'reg-repeat "exact-nonnegative-integer?" 0 lo hi rset))
  (unless (or (exact-nonnegative-integer? hi) (= hi +inf.0))
    (raise-argument-error 'reg-repeat "exact-nonnegative-integer?" 1 lo hi rset))
  (unless (<= lo hi)
    (raise-argument-error 'reg-repeat "range not valid" 2 lo hi rset))
  (unless (kindof-regular? rset)
    (raise-argument-error 'reg-repeat "kindof-regular?" 2 lo hi rset))
  (cond [(= hi +inf.0)
         (reg-conc (apply reg-conc (make-list lo rset))
                   (reg-kleene rset))]
        [else
         (let recur ([i lo]
                     [rset* (apply reg-conc (make-list lo rset))])
           (if (> i hi)
               (nothing)
               (reg-union rset* (recur (add1 i) (reg-conc rset rset*)))))]))

(provide just-string reg-maybe reg-repeat)

;;;
;;; COERCION AND TYPE PREDICATES
;;;

;;; unicode-codepoint? : any? -> boolean?
;;; 
;;; Check if the given argument is an exact integer that can be
;;; converted to a character.
(define (unicode-codepoint? x)
  (and (exact-integer? x)
       (or (<= 0 x #xD7FF)
           (<= #xE000 x #x10FFFF))))

;;; coerce-char : any? -> char? or #f
;;;
;;; Returns a char from the given argument, #f if the conversion is
;;; not supported.
(define (coerce-char x)
  (cond [(char? x) x]
        [(unicode-codepoint? x)
         (integer->char x)]
        [(and (string? x) (= 1 (string-length x)))
         (string-ref x 0)]
        [else #f]))

;;; regular? : any? -> boolean?
;;;
;;; Check if the given argument can be treated like a regular
;;; language.
(define (kindof-regular? x)
  (or (regular? x)
      (char? x)
      (string? x)
      (unicode-codepoint? x)))

(provide (rename-out [kindof-regular? regular?]))

;;; ensure-regular : any? -> regular?
(define (ensure-regular who x)
  (cond [(regular? x) x]
        [(string? x) (just-string x)]
        [(coerce-char x) => just-char]
        [else
         (raise-argument-error 'who "kindof-regular?" x)]))

(provide ensure-regular)

;;; Everything is the Kleene closure of the alphabet

(define +everything+ (reg-comp (nothing)))
(define (everything) +everything+)
(define (everything? x) (eq? x +everything+))

(provide everything everything?)
