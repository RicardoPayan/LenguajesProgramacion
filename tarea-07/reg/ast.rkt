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
;;;      ~ Abstract Syntax Trees ~

#lang racket/base

;;; Abstract structure for regular languages
(struct regular () #:transparent)

;;; An empty set
(struct nothing regular () #:transparent)

;;; A singleton set with the empty string
(struct just-epsilon regular () #:transparent)

;;; A singleton set with the given char
(struct just-char regular (value)
  #:transparent
  #:guard (lambda (c type)
            (unless (char? c)
              (raise-argument-error type "char?" c))
            c))

;;; A finite set of chars
(struct char-set regular (predicate)
  #:transparent
  #:constructor-name make-char-set)

;;; Kleene closure of the given set
(struct reg-kleene regular (set)
  #:transparent
  #:constructor-name make-reg-kleene)

;;; Set complement of the given set
(struct reg-comp regular (set)
  #:transparent
  #:constructor-name make-reg-comp)

;;; Union of left and right sets
(struct reg-union regular (left right)
  #:transparent
  #:constructor-name make-reg-union)

;;; Intersection of left and right sets
(struct reg-inter regular (left right)
  #:transparent
  #:constructor-name make-reg-inter)

;;; Concatenation of left and right sets
(struct reg-conc regular (left right)
  #:transparent
  #:constructor-name make-reg-conc)

(provide regular?
         nothing nothing?
         just-epsilon just-epsilon?
         just-char just-char? just-char-value
         make-char-set char-set? char-set-predicate
         make-reg-kleene reg-kleene? reg-kleene-set
         make-reg-comp reg-comp? reg-comp-set
         make-reg-union reg-union? reg-union-left reg-union-right
         make-reg-inter reg-inter? reg-inter-left reg-inter-right
         make-reg-conc reg-conc? reg-conc-left reg-conc-right)
