#lang plait

(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str)) empty-env))


;Definicion de tipos-----------------
(define-type Value
  (numV [value : Number])
  (strV [value : String])
  (boolV [value : Boolean])
  (funV [param : Symbol] [body : ExprC]))

(define-type Operator
  (plus0)
  (append0)
  (numeq0)
  (streq0))

(define-type ExprC
  (numC [n : Number])
  (strC [s : String])
  (boolC [b : Boolean])
  (idC [name : Symbol])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (binopC [op : Operator] [left : ExprC] [right : ExprC])
  (funC [name : Symbol] [body : ExprC])
  (appC [func : ExprC] [arg : ExprC]))


(define-type ExprS
  (numS [n : Number])
  (strS [s : String])
  (boolS [b : Boolean])
  (idS [name : Symbol] )
  (binopS [op : Operator] [left : ExprS] [right : ExprS]) 
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [left : ExprS] [right : ExprS])
  (orS [left : ExprS] [right : ExprS])
  (funS [name : Symbol] [body : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (appS [func : ExprS][arg : ExprS]) )

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "unbound identifier: "

          (to-string name))))


;---Environment---------------
(define-type Binding
  (binding [name : Symbol]
           [value : Value]))

(define-type-alias Environment (Listof Binding))

(define empty-env empty)

(define (lookup-env name env)
  (if (empty? env)
      (unbound-identifier-error name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (lookup-env name (rest env)))))

(define (extend-env name value env)
  (cons (binding name value) env))

;---Desugar------------------------

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(boolS b) (boolC b)]
    [(strS s) (strC s)]
    [(idS name) (idC name)]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(binopS op e2 e3) (binopC op (desugar e2) (desugar e3))]
    [(andS e1 e2) (ifC (desugar e1) (desugar e2) (boolC #f))]
    [(orS e1 e2) (ifC (desugar e1) (boolC #t) (desugar e2))]
    [(funS name body) (funC name (desugar body))]
    [(letS name value body) (appC (funC name (desugar body)) (desugar value))]
    [(appS fun arg) (appC (desugar fun) (desugar arg))]))


;---Interprete-----------
(define (interp [e : ExprC] [env : Environment]) : Value
   (interp-helper e env))

(define (interp-helper [e : ExprC] [env : Environment]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(boolC b) (boolV b)]
    [(strC s) (strV s)]
    [(idC name) (lookup-env name env)]
    [(ifC a b c)
     (let ([v1 (interp-helper a env)])
       (cond
         [(not (boolV? v1))
          (bad-conditional-error v1)]
         [(boolV-value v1) (interp-helper b env)]
         [else (interp-helper c env)]))]
    [(binopC op left right)
             (let ([left (interp-helper left env)])
               (let ([right (interp-helper right env)])
                 (interp-binop op left right)))]
    [(funC name body) (funV name body)]
    [(appC func arg)
     (let ([v1 (interp-helper func env)])
       (cond
         [(not (funV? v1))
          (bad-app-error v1)]
         [else (interp-helper arg env)]))]))


(define (interp-binop [op : Operator]
                      [left : Value]
                      [right : Value]) : Value
  (type-case Operator op
    [(plus0)
     (cond
       [(boolV? left) (error 'binop "No se pueden sumar booleanos")]
       [(boolV? right) (error 'binop "No se pueden sumar booleanos")]
       [else (cond
       [(numV? left)
        (cond
          [(numV? right)
           (numV (+ (numV-value left) (numV-value right)))]
          [else (error 'binop "El derecho no es numero, argumento incorrecto")])]
       [else (error 'binop "El izquierdo no es numero, argumento incorrecto")])])]
    [(append0)
     (cond
       [(strV? left)
        (cond
          [(strV? right)
           (strV (string-append (strV-value left) (strV-value right)))]
          [else (error 'binop "El derecho no es un string")])]
       [else (error 'binop "El izquierdo no es un string")])]
    [(numeq0)
     (cond
       [(numV? left)
        (cond
          [(numV? right)
           (boolV (= (numV-value left) (numV-value right)))]
          [else (error 'binop "El derecho no es un numero")])]
       [else (error 'binop "El izquierdo no es un numero")])]
    [(streq0)
     (cond
       [(strV? left)
        (cond
          [(strV? right)
           (boolV (string=? (strV-value left) (strV-value right)))]
          [else (error 'binop "El derecho no es un string")])]
       [else (error 'binop "El izquierdo no es un string")])]))



;---Mensajes de error---------

(define (bad-conditional-error [v : Value])
  (error 'interp
         (string-append
          "Condicional mal formado para IF expression: "
          (to-string v))))

(define (bad-app-error [v : Value])
  (error 'interp
         (string-append
          "Aplicacion mal formada, el valor no es una funcion: "
          (to-string v))))



     
;----Parse---------
(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in)
     (parse-number in)]
    [(s-exp-string? in)
     (parse-string in)]
    [(s-exp-match? `true in)
     (boolS #t)]
    [(s-exp-match? `false in)
     (boolS #f)]
    [(s-exp-match? `{if ANY ...} in)
     (parse-if in)]
    [(s-exp-match? `{and ANY ...} in)
     (parse-and in)]
    [(s-exp-match? `{or ANY ...} in)
     (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in)
     (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in)
     (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in)
     (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in)
     (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in)
     (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in)
     (parse-app in)]
    [(s-exp-symbol? in)
     (parse-id in)]))

;Funciones para parsear valores ---------------------
(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plus0) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (append0) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeq0) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streq0) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de
               funciones"))))
;----------------------------------------------------