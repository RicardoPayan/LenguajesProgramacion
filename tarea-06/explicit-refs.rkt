#lang racket

#|

SINTAXIS CONCRETA
=================

Program := Expression
Expression := Number
Expression := -(Expression , Expression)
Expression := zero? (Expression)
Expression := if Expression then Expression else Expression
Expression := Identifier
Expression := let Identifier = Expression in Expression
Expression := proc (Identifier) Expression
Expression := (Expression Expression)

SINTAXIS CONCRETA (S-Expressions)
=================================

Program := Expression
Expression := Number
Expression := (- Expression Expression)
Expression := (zero? Expression)
Expression := (if Expression Expression Expression)
Expression := Identifier
Expression := (let (Identifier Expression) Expression)
Expression := (proc Identifier Expression)
Expression := (Expression Expression)

SINTAXIS ABSTRACTA
==================

Programa:
- (a-program exp1)

Expresión:
- (const-exp num)
- (diff-exp exp1 exp2)
- (zero?-exp exp1)
- (if-exp exp1 exp2 exp3)
- (var-exp var)
- (let-exp var exp1 body)
- (proc-exp var body)
- (call-exp rator rand)
-(newref-exp exp1)
-(deref-exp exp1)
-(set-ref exp1 exp2)

|#

(struct a-program (exp1)
  #:transparent)

(define (program? x)
  (a-program? x))

(struct const-exp (num)
  #:transparent)

(struct diff-exp (exp1 exp2)
  #:transparent)

(struct zero?-exp (exp1)
  #:transparent)

(struct if-exp (exp1 exp2 exp3)
  #:transparent)

(struct var-exp (var)
  #:transparent)

(struct let-exp (var exp1 body)
  #:transparent)

(struct proc-exp (var body)
  #:transparent)

(struct call-exp (rator rand)
  #:transparent)

(struct newref-exp (exp1)
  #:transparent)

(struct deref-exp (exp1)
  #:transparent)

(struct setref-exp (ref exp)
  #:transparent)

(struct result (value state)
  #:transparent)

(struct letrec-exp (p-name b-var p-body letrec-body)
  #:transparent)

(define (expression? x)
  (or (const-exp? x)
      (diff-exp? x)
      (zero?-exp? x)
      (if-exp? x)
      (var-exp? x)
      (let-exp? x)
      (proc-exp? x)
      (call-exp? x)
      (newref-exp? x)
      (deref-exp? x)
      (setref-exp? x)))

;; parse : S-Expression -> program?
;; Traducción de S-Expressions a sintaxis abstracta de LET
(define (parse x)
  (a-program (parse-expression x)))

;; parse-expression : S-Expression -> expression?
(define (parse-expression x)
  (cond
    [(number? x) (const-exp x)]
    [(symbol? x) (var-exp x)]
    [(pair? x)
     (case (first x)
       [(-) (parse-diff x)]
       [(zero?) (parse-zero? x)]
       [(if) (parse-if x)]
       [(let) (parse-let x)]
       [(proc) (parse-proc x)]
       [(newref) (parse-newref x)]
       [(deref) (parse-deref x)]
       [(setref) (parse-setref x)]
       [else
        (if (and (list? x)
                 (= 2 (length x)))
            (parse-call x)
            (error 'parse "expresión no es válida: ~e" x))])]
    [else
     (error 'parse "expresión no es válida: ~e" x)]))

;; parse-diff : pair? -> diff-exp?
;; x es un par de la forma (- . _)
(define (parse-diff x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (diff-exp (parse-expression (second x))
            (parse-expression (third x))))

;; parse-zero? : pair? -> zero?-exp?
;; x es un par de la forma (zero? . _)
(define (parse-zero? x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (zero?-exp (parse-expression (second x))))

;; parse-if : pair? -> if-exp?
;; x es un par de la forma (if . _)
(define (parse-if x)
  (unless (= (length x) 4)
    (error 'parse "expresión no es válida: ~e" x))
  (if-exp (parse-expression (second x))
          (parse-expression (third x))
          (parse-expression (fourth x))))

;; parse-let : pair? -> let-exp?
;; x es un par de la forma (let . _)
(define (parse-let x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (let ([binding (second x)])
    (unless (= (length binding) 2)
      (error 'parse "expresión no es válida: ~e" x))
    (unless (symbol? (first binding))
      (error 'parse "expresión no es válida: ~e" x))
    (let-exp (first binding)
             (parse-expression (second binding))
             (parse-expression (third x)))))

;; parse-proc : pair? -> proc-exp?
;; x es un par de la forma (proc . _)
(define (parse-proc x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (unless (symbol? (second x))
    (error 'parse "expresión no es válida: ~e" x))
  (proc-exp (second x)
            (parse-expression (third x))))

;; parse-call : list? -> call-exp?
;; x es una lista de dos elementos
(define (parse-call x)
  (call-exp (parse-expression (first x))
            (parse-expression (second x))))


(define (parse-newref x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (newref-exp (parse-expression (second x))))

(define (parse-deref x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (deref-exp (parse-expression (second x))))

(define (parse-setref x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  ( setref-exp (parse-expression (second x))
               (parse-expression (third x))))


 
;;;;;;;;;;;;;;
;; ENTORNOS ;;
;;;;;;;;;;;;;;

(define (empty-env)
  null)

(define (apply-env env var)
  (if (null? env)
      (error 'environment "variable libre: ~e" var)
      (let ([binding (first env)])
        (if (equal? var (first binding))
            (result (second binding) (get-store))
            (apply-env (rest env) var)))))

(define (extend-env var val env)
  (cons (list var val) env))

(define (extend-env-rec p-name b-var body saved-env)
  (let ((vec (make-vector 1)))
    (let ((new-env (extend-env p-name vec saved-env)))
      (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
      new-env)))

;;;;;;;;;;;;;;;;;;;;
;; PROCEDIMIENTOS ;;
;;;;;;;;;;;;;;;;;;;;

(struct procedure (var body saved-env)
  #:transparent)

(define (apply-procedure proc val st)
  (unless (procedure? proc)
    (error 'value-of "no es un procedimiento: ~e" proc))
  (let ([var (procedure-var proc)]
        [body (procedure-body proc)]
        [saved-env (procedure-saved-env proc)])
    (value-of body (extend-env var val saved-env) st)))

;;;;;;;;;;;;;;
;;   STORE  ;;
;;;;;;;;;;;;;;
(define (empty-store)
  null)

(define the-store 'dummy)

(define (get-store) the-store)

(define (initialize-store!)
  (set! the-store(empty-store)))

(define (reference? v)
  (integer? v))



;;;;;;;;;;;;;;;
;; SEMÁNTICA ;;
;;;;;;;;;;;;;;;

#|

VALORES EXPRESADOS Y DENOTADOS

ExpVal = Int + Bool + Proc
DenVal = Int + Bool + Proc

|#

(struct num-val (num)
  #:transparent
  #:guard (lambda (num type-name)
            (unless (number? num)
              (error type-name "no es un número: ~e" num))
            num))

(define expval->num num-val-num)

(struct bool-val (bool)
  #:transparent
  #:guard (lambda (bool type-name)
            (unless (boolean? bool)
              (error type-name "no es un booleano: ~e" bool))
            bool))

(define expval->bool bool-val-bool)

(struct proc-val (proc)
  #:transparent
  #:guard (lambda (proc type-name)
            (unless (procedure? proc)
              (error type-name "no es un procedimiento: ~e" proc))
            proc))

(define expval->proc proc-val-proc)

(struct ref-val (ref)
  #:transparent
  #:guard (lambda (ref type-name)
            (unless (integer? ref)
              (error type-name "no es una referencia ~e" ref))
            ref))

(define expval->ref ref-val-ref)

(struct pair-val (pair)
  #:transparent
  #:guard (lambda (pair type-name)
            (unless (pair? pair)
              (error type-name "no es un par ~e" pair))
            pair))

(define expval->pair pair-val-pair)

#|

ESPECIFICACIONES SEMÁNTICAS

(value-of (const-exp n) env) = (num-val n)

(value-of (var-exp var) env) = (apply-env env var)

(value-of (diff-exp exp1 exp2) env)
 = (num-val
    (- (expval->num (value-of exp1 env))
       (expval->num (value-of exp2 env))))

(value-of (zero?-exp exp1) env)
 = (if (equal? 0 (expval->num (value-of exp1 env)))
       (bool-val #t)
       (bool-val #f))

(value-of (if-exp exp1 exp2 exp3) env)
 = (if (expval->bool (value-of exp1 env))
       (value-of exp2 env)
       (value-of exp3 env))

(value-of (let-exp var exp1 body) env)
 = (value-of body (extend-env var (value-of exp1 env) env))

(value-of (proc-exp var body) env)
 = (proc-val (procedure var body env))

(value-of (call-exp rator rand) env)
 = (let ([proc (expval->proc (value-of rator env))]
         [arg (value-of rand env)])
     (apply-procedure proc arg))

|#

(define (value-of exp env st)
  (cond
    [(const-exp? exp)
     (let ([n (const-exp-num exp)])
       (result (num-val n) st))]
    [(var-exp? exp)
     (let ([var (var-exp-var exp)]) 
       (apply-env env var))]
    [(diff-exp? exp)
     (let ([exp1 (diff-exp-exp1 exp)]
           [exp2 (diff-exp-exp2 exp)])
       (let ([r1 (value-of exp1 env st)])
         (let ([r2 (value-of exp2 env (result-state r1))])
               (result (num-val (- (expval->num (result-value r1)) (expval->num (result-value r2))))
                       (result-state r2)))))]
    [(zero?-exp? exp)
     (let* ([exp1 (zero?-exp-exp1 exp)]
            [r1 (value-of exp1 env st)])
       (if (equal? 0 (expval->num (result-value r1)))
           (result (bool-val #t) st)
           (result (bool-val #f) st)))]
    [(if-exp? exp)
     (let ([exp1 (if-exp-exp1 exp)]
           [exp2 (if-exp-exp2 exp)]
           [exp3 (if-exp-exp3 exp)])
       (let ([r1 (value-of exp1 env st)])
         (if (expval->bool (result-value r1))
           (value-of exp2 env (result-state r1))
           (value-of exp3 env (result-state r1)))))]
    [(let-exp? exp)
     (let ([var (let-exp-var exp)]
           [exp1 (let-exp-exp1 exp)]
           [body (let-exp-body exp)])
       (let ([r1 (value-of exp1 env st)])
         (value-of body (extend-env var (result-value r1) env) (result-state r1))))]
    [(proc-exp? exp)
     (let ([var (proc-exp-var exp)]
           [body (proc-exp-body exp)])
       (result (proc-val (procedure var body env)) st))]
    [(call-exp? exp)
     (let* ([rator (call-exp-rator exp)]
           [rand (call-exp-rand exp)]
           [r1 (value-of rator env st)]
           [proc (expval->proc (result-value r1))]
           [r2 (value-of rand env (result-state r1))]
           [arg (result-value r2)])
         (apply-procedure proc arg (result-state r2)))]
    [(newref-exp? exp)
     (let* ([this-ref (length st)]
            [r1 (value-of (newref-exp-exp1 exp) env st)])
       (set! the-store (append the-store
                                (list (result-value r1))))
       (result (ref-val this-ref) the-store))]
    [(deref-exp? exp)
     (let* ([r1 (value-of (deref-exp-exp1 exp) env st)])
         (result (list-ref (result-value r1) the-store) (result-state r1)))]
    [(setref-exp? exp)
     (result setref-exp-exp (append (take st setref-exp-ref)
                                    
                            (list (result-value (value-of setref-exp-exp env st)))
                            (drop st (+ (expval->num setref-exp-ref) 1))))]
    [(letrec-exp? exp)
     (let* ([p-name (letrec-exp-p-body exp)]
            [b-var (letrec-exp-b-var exp)]
            [p-body (letrec-exp-p-body exp)]
            [letrec-body (letrec-exp-letrec-body exp)])
       (value-of letrec-body (extend-env-rec p-name b-var p-body env) st))]
    [else
     (error 'value-of "no es una expresión: ~e" exp)]))

(define (init-env)
  (foldl (lambda (binding env)
           (extend-env (first binding) (second binding) env))
         (empty-env)
         (list (list 'π (num-val 3.141592653589793))
               (list 'e (num-val 2.718281828459045))
               (list 'i (num-val 0+1i))
               (list 'G (num-val 6.674e-11))
               (list 'c (num-val 299792458))
               (list 'h (num-val 6.62607015e-34)))))

(define (value-of-program pgm)
  (initialize-store!)
  (if (program? pgm)
      (let ([exp1 (a-program-exp1 pgm)])
          (value-of exp1 (init-env) the-store))
      (error 'value-of-program "no es un programa: ~e" pgm)))

(define (run sexp)
  (value-of-program (parse sexp)))