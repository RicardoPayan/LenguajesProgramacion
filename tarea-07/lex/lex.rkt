;;; -*- mode: racket; coding: utf-8 -*-
;;; 
;;; `7MMF'      `7MM"""YMM  `YMM'   `MP' 
;;;   MM          MM    `7    VMb.  ,P   
;;;   MM          MM   d       `MM.M'    
;;;   MM          MMmmMM         MMb     
;;;   MM      ,   MM   Y  ,    ,M'`Mb.   
;;;   MM     ,M   MM     ,M   ,P   `MM.  
;;; .JMMmmmmMMM .JMMmmmmMMM .MM:.  .:MMa.
;;; 
;;;    ~ Lexical Analyzer Generator ~

#lang racket/base

(require racket/function
         racket/port

         "../reg/reg.rkt")

(struct lex-rule (set action)
  #:transparent
  #:guard (lambda (rset action type)
            (unless (regular? rset)
              (raise-argument-error type "regular?" 0 rset action))
            (unless (procedure? action)
              (raise-argument-error 'lex-rule "procedure?" 1 rset action))
            (unless (procedure-arity-includes? action 4)
              (error 'lex-rule "not a procedure that takes four arguments: ~v" action))
            (values (ensure-regular 'lex-rule rset)
                    action)))

(struct pos (line col offset)
  #:transparent)

(define (deriv-rule rule c)
  (lex-rule (reg-deriv (lex-rule-set rule) c)
            (lex-rule-action rule)))

(define (rejecting? rule)
  (nothing? (lex-rule-set rule)))

(define (all-rejecting? rules)
  (or (null? rules)
      (and (rejecting? (car rules))
           (all-rejecting? (cdr rules)))))

(define (accepting? rule)
  (nullable? (lex-rule-set rule)))

(define (some-action rules)
  (cond [(null? rules) #f]
        [(accepting? (car rules)) (lex-rule-action (car rules))]
        [else (some-action (cdr rules))]))

(define (source-position src)
  (define-values (line col offset) (port-next-location src))
  (pos line col offset))

(define lexer-name (make-parameter #f))

;;; Create a lexer procedure with the given name and any number of
;;; rules, the resulting procedure takes an input port and dispatches
;;; the matched rule action.
(define (make-lexer name . rules)
  (unless (or (not name) (symbol? name))
    (raise-argument-error 'make-lexer "symbol?" name))
  (define (lexer src)
    (unless (input-port? src)
      (raise-argument-error 'lexer "input-port?" 0 src))
    (if (eof-object? (peek-char src))
        eof
        (parameterize ([lexer-name (if name name 'lexer)])
          (lex-match src (source-position src) (peek-char src) rules #f 0 0 0))))
  lexer)

;;; Call longest match action, consuming match bytes from input src.
;;; src : input-port?                   [current input source]
;;; beg : pos                           [lexer start position]
;;; ch : (or char? eof-object?)         [char peeked from input]
;;; rules : (list-of lex-rule?)         [rules to match against]
;;; maction : procedure?                [longest match action]
;;; mchars : exact-nonnegative-integer? [longest match number of chars]
;;; nbytes : exact-nonnegative-integer? [number of peeked bytes from input]
;;; nchars : exact-nonnegative-integer? [number of peeked chars from input]
(define (lex-match src beg ch rules maction mchars nbytes nchars)
  (define next-rules
    (and (char? ch)
         (map (curryr deriv-rule ch) rules)))
  (cond [(or (not next-rules)
             (all-rejecting? next-rules))
         (lex-dispatch src beg maction mchars nchars)]
        [else
         (define action (some-action next-rules))
         (define next-nbytes (+ nbytes (char-utf-8-length ch)))
         (lex-match src beg
                    (peek-char src next-nbytes)
                    next-rules
                    (if action action maction)
                    (if action (add1 nchars) mchars)
                    next-nbytes
                    (add1 nchars))]))
;;; Dispatch matched action
;;; src : input-port? [current input source]
;;; beg : pos [lexer start position]
;;; maction : procedure? [longest match action]
;;; mchars : exact-nonnegative-integer? [longest match number of chars]
;;; nchars : exact-nonnegative-integer? [number of peeked chars from input]
(define (lex-dispatch src beg maction mchars nchars)
  (unless maction
    (define prefix (read-string (add1 nchars) src))
    (define end (source-position src))
    (error (lexer-name)
           "~a: lexema inesperado ~v"
           (srcloc->string
            (srcloc (object-name src)
                    (pos-line beg)
                    (pos-col beg)
                    (pos-offset beg)
                    (- (pos-offset end) (pos-offset beg))))
           prefix))
  (let* ([lexeme (read-string mchars src)]
         [end (source-position src)])
    (maction src lexeme beg end)))

(provide (struct-out pos)
         lex-rule
         make-lexer
         (all-from-out "../reg/reg.rkt"))
