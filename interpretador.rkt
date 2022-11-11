#lang eopl

#|
    Diseñe un interpretador para la siguiente gramática que realiza
    operaciones con notación infija:
    Valores denotados: Texto + Número + Booleano + ProcVal
    Valores expresado: Texto + Número + Booleano + ProcVal
|#

#|
    La definición BNF para las expresiones del lenguaje:

    <programa> :=  <expresion> 
               un-programa (exp)

    <expresion> := <numero>
                numero-lit (num)
                := "\""<texto> "\""
                texto-lit (txt)
                := <identificador>
                var-exp (id)
                := (<expresion> <primitiva-binaria> <expresion>)
                primapp-bin-exp (exp1 prim-binaria exp2)
                := <primitiva-unaria> (<expresion>)
                primapp-un-exp (prim-unaria exp)
                := "Si" <expresion> "entonces" <expresion> "sino" <expresion> "finSi"
                condicional-exp(test-exp true-exp false-exp)
                := "declarar" "(" <identificador> "=" <expresion> (";") ")" "{" <expresion> "}"
                variableLocal-exp(ids exps cuerpo)

    <primitiva-binaria> :=  + (primitiva-suma)
                        :=  ~ (primitiva-resta)
                        :=  / (primitiva-div)
                        :=  * (primitiva-multi)
                        :=  concat (primitiva-concat)

    <primitiva-unaria> :=  longitud (primitiva-longitud)
                       :=  add1 (primitiva-add1)
                       :=  sub1 (primitiva-sub1)
|#

#|
    Tenga en cuenta que:
    <numero>: Debe definirse para valores decimales y enteros (positivos y negativos)
    <texto>: Debe definirse para cualquier texto escrito en racket
    <identificador>: En este lenguaje todo identificador iniciará con el símbolo  @, es decir las
                     variables @x y @z son válidas
|#

;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
    ("%"(arbno (not #\newline))) skip)
  ;pregunta como colocar \ \ y letras y numeros al tiempo
  (texto
   ("/" (arbno (or letter digit)) "/") string)
  ;pregunta solo debe ser valido un ? y cómo se haría
  (identificador
   ;;("@" (arbno (or letter digit)) "?") symbol)
   ("@" letter (arbno (or letter digit "?"))) symbol)
  ; enteros positivos y negativos
  (numero 
   (digit (arbno digit)) number)
  (numero 
   ("-" digit (arbno digit)) number)
  ; flotantes positivos y negativos
  (numero 
   (digit (arbno digit) "." digit (arbno digit)) number)
  (numero 
   ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numero) numero-lit)
    (expression (texto) texto-lit)
    (expression (identificador) var-exp)
    (expression (primitiva-unaria "("expression")") primapp-un-exp)
    (expression ("("expression primitiva-binaria expression")") primapp-bin-exp)
    
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("concat") primitiva-concat)

    (expression ("Si" expression "entonces" expression "sino" expression "finSI") condicional-exp)
    (expression ("declarar" "(" (arbno identificador "=" expression ";") ")" "{" expression "}") variableLocal-exp)

    ;; Extendiendo procedimientos
    (expression ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expression "finProc" )procedimiento-exp)
    ;;Extendiendo para Evaluar procedimientos
    ;;Al final me queda una comita (QUITARLA)
    (expression ("evaluar" expression "("(arbno expression "," ) ")" "finEval" ) app-exp)

    ;;Extendiendo para hacer llamados recursivos
    ;;(expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression)  "in" expression) declaracion-rec)
    ;;Se escribio la gramatica de tal manera que quedara lo mas parecido a declarar
    (expression ("declaracion-rec" (arbno identificador "(" (separated-list identificador ",") ")" "=" expression) "{" expression "}") declaracion-rec)

    
   )
)

;Tipos de datos para la sintaxis abstracta de la gramática

;Construidos manualmente:

;(define-datatype program program?
;  (a-program
;   (exp expression?)))
;
;(define-datatype expression expression?
;  (lit-exp
;   (datum number?))
;  (var-exp
;   (id symbol?))
;  (primapp-exp
;   (prim primitive?)
;   (rands (list-of expression?)))
;  (if-exp
;   (test-exp expression?)
;   (true-exp expression?)
;   (false-exp expression?))
;  (let-exp
;   (ids (list-of symbol?))
;   (rans (list-of expression?))
;   (body expression?)))
;
;(define-datatype primitive primitive?
;  (add-prim)
;  (substract-prim)
;  (mult-prim)
;  (incr-prim)
;  (decr-prim))

;Construida automáticamente la sintaxis abstracta:

(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--❤ "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;*******************************************************************************************
;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))
(define init-env
  (lambda ()
    (extend-env
     ;;'(@x @y @z)
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (primapp-un-exp (prim-unaria exp)
                      (apply-un-primitive prim-unaria (eval-expression exp env)))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (apply-bin-primitive (eval-expression exp1 env) prim-binaria (eval-expression exp2 env)))

      (condicional-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      ;;(variableLocal-exp (ids exps cuerpo) (0))
      (variableLocal-exp (ids exps cuerpo)
               (let ((args (eval-rands exps env)))
                 (eval-expression cuerpo
                                  (extend-env ids args env))))
      
      (procedimiento-exp (ids cuerpo)
                         (cerradura ids cuerpo env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      

      ;Recursividad
      (declaracion-rec (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      ))
)
; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-un-primitive: <primitiva-unaria> (<expression>) -> numero
(define apply-un-primitive
  (lambda (prim-unaria exp)
    (cases primitiva-unaria prim-unaria
      (primitiva-longitud ()(- (string-length exp) 2))
      (primitiva-add1 () (+ exp 1))
      (primitiva-sub1 () (- exp 1)))))

;apply-bin-primitive: (<expression> <primitiva-binaria> <expression>) -> numero
(define apply-bin-primitive
  (lambda (exp1 prim-binaria exp2)
    (cases primitiva-binaria prim-binaria
      (primitiva-suma () (+ exp1 exp2))
      (primitiva-resta () (- exp1 exp2))
      (primitiva-multi () (* exp1 exp2))
      (primitiva-div () (/ exp1 exp2))
      (primitiva-concat () (string-append (number->string exp1) (number->string exp2)))
      
      )))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))



;*******************************************************************************************
;Procedimientos
(define-datatype procval procval?
  (cerradura
   (list-ID (list-of symbol?))
   (exp expression?)
   (amb environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids cuerpo env)
               (eval-expression cuerpo (extend-env ids args env))))))

;*******************************************************************************************


;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))

                       (env environment?))
  ;;Recursivo
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?))
  )

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env env sym))))
      ;;Recursivo
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym))))
      )))


;****************************************************************************************
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;******************************************************************************************
;EJERCICIOS