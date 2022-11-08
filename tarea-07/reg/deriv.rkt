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
;;;   ~ Regular Language Derivative ~

#lang racket/base

(require "cons.rkt")

;;; Filter epsilon from given set
(define (nullable? rset)
  (cond [(nothing? rset) rset]
        [(just-epsilon? rset) rset]
        [(just-char? rset) (nothing)]
        [(char-set? rset) (nothing)]
        [(reg-kleene? rset)
         (just-epsilon)]
        [(reg-comp? rset)
         (if (just-epsilon? (nullable? (reg-comp-set rset)))
             (nothing)
             (just-epsilon))]
        [(reg-union? rset)
         (reg-union (nullable? (reg-union-left rset))
                    (nullable? (reg-union-right rset)))]
        [(reg-inter? rset)
         (reg-inter (nullable? (reg-inter-left rset))
                    (nullable? (reg-inter-right rset)))]
        [(reg-conc? rset)
         (reg-inter (nullable? (reg-conc-left rset))
                    (nullable? (reg-conc-right rset)))]
        [else
         (raise-argument-error 'nullable? "regular?" 0 rset)]))

;;; Predicate to check if the given set contains epsilon
(define (really-nullable? rset)
  (just-epsilon? (nullable? rset)))

;;; Given a set rset and a character c, returns the smallest regular
;;; set of all strings β, where c⋅β is in reg.
(define (deriv rset c)
  (cond [(nothing? rset) (nothing)]
        [(just-epsilon? rset) (nothing)]
        [(just-char? rset)
         (if (char=? (just-char-value rset) c)
             (just-epsilon)
             (nothing))]
        [(char-set? rset)
         (if ((char-set-predicate rset) c)
             (just-epsilon)
             (nothing))]
        [(reg-kleene? rset)
         (reg-conc (deriv (reg-kleene-set rset) c) rset)]
        [(reg-comp? rset)
         (reg-comp (deriv (reg-comp-set rset) c))]
        [(reg-union? rset)
         (reg-union (deriv (reg-union-left rset) c)
                    (deriv (reg-union-right rset) c))]
        [(reg-inter? rset)
         (reg-inter (deriv (reg-inter-left rset) c)
                    (deriv (reg-inter-right rset) c))]
        [(reg-conc? rset)
         (reg-union (reg-conc (deriv (reg-conc-left rset) c)
                              (reg-conc-right rset))
                    (reg-conc (nullable? (reg-conc-left rset))
                              (deriv (reg-conc-right rset) c)))]
        [else
         (raise-argument-error 'reg-deriv "regular?" 0 rset)]))

;;; Like deriv but also for strings
(define (carefully-deriv rset x)
  (unless (regular? rset)
    (raise-argument-error 'reg-deriv "regular?" 0 rset x))
  (let ([rset* (ensure-regular 'reg-deriv rset)])
    (cond [(char? x)
           (deriv rset* x)]
          [(string? x)
           (let loop ([i 0] [rset* rset*])
             (if (>= i (string-length x))
                 rset*
                 (loop (add1 i) (deriv rset* (string-ref x i)))))]
          [else
           (raise-argument-error 'reg-deriv "char?" 1 rset x)])))

(provide (rename-out [carefully-deriv reg-deriv])
         (rename-out [really-nullable? nullable?]))
