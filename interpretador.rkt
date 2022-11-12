#lang eopl
#|
    Integrantes:
    César Alejandro Grijalba Zúñiga - 202110035
    Johan Sebastian Tombe - 202110051
    Laura Murillas Andrade - 1944153
    Juan Esteban Mazuera - 2043008
    Sheilly Ortega - 2040051

    Repositorio de GitHub: https://github.com/DaylinSheilly/interpretador.git
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
   ("\"" (arbno (or letter digit whitespace "-" ":")) "\"") string)
  ;pregunta solo debe ser valido un ? y cómo se haría
  (identificador
   ("@" (arbno (or letter digit "?"))) symbol)
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

    (expression ("procedimiento" "(" (separated-list identificador ",") ")" "haga" expression "finProc" )procedimiento-exp)
    (expression ("evaluar" expression "("(separated-list expression "," ) ")" "finEval" ) app-exp)
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

(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e @f)
     '(1 2 3 "hola" "FLP")
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable env id)) ;por aqui entra
      (primapp-un-exp (prim-unaria exp)
                      (apply-un-primitive prim-unaria exp env))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (apply-bin-primitive exp1 prim-binaria exp2 env))
      (condicional-exp (test-exp true-exp false-exp)
                       (if(true-value? (eval-expression test-exp env))
                          (eval-expression true-exp env)
                          (eval-expression false-exp env)
                        ))
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
      
      (declaracion-rec (proc-names idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      )))

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
  (lambda (prim-unaria exp env)
    (cases primitiva-unaria prim-unaria
      (primitiva-longitud () (medir-texto exp env))
      (primitiva-add1 () (+ (eval-expression exp env) 1))
      (primitiva-sub1 () (- (eval-expression exp env) 1)))))

;apply-bin-primitive: (<expression> <primitiva-binaria> <expression>) -> numero
;apply-bin-primitive: (<expression> <primitiva-binaria> <expression>) -> numero
(define apply-bin-primitive
  (lambda (exp1 prim-binaria exp2 env)
    (cases primitiva-binaria prim-binaria
      (primitiva-suma () (+ (eval-expression exp1 env) (eval-expression exp2 env)))
      (primitiva-resta () (- (eval-expression exp1 env) (eval-expression exp2 env)))
      (primitiva-multi () (* (eval-expression exp1 env) (eval-expression exp2 env)))
      (primitiva-div () (/ (eval-expression exp1 env) (eval-expression exp2 env)))
      (primitiva-concat () (string-append (recortar-string exp1 env) (recortar-string exp2 env)))
    )
  )
)

;medir-texto: <string> -> <number>
(define medir-texto
  (lambda (exp env)
    (cases expression exp
      (texto-lit (txt) (-(string-length (eval-expression exp env))2))
      (var-exp (id) (string-length (eval-expression exp env)))
      (else (0))
    )
  )
)

;recortar-string: <string> -> <string>
(define recortar-string
  (lambda (exp env)
    (cases expression exp
      (texto-lit (txt) (substring (eval-expression exp env) 1 (- (string-length (eval-expression exp env)) 1)))
      (else (eval-expression exp env))
    )
  )
)

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;************************************************************************************************
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
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))

                       (env environment?))
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

;función que busca un identificador en un ambiente
;Cambiar simbolo por identificador
(define buscar-variable
  (lambda (env idn) ;idn es simbolo a buscar
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "Error la variable no existe: ~s" idn))
      (extended-env-record (lista-idn vals env) ;lista-idn es lista de simbolos - vals es lista de valores
                           (let ((pos (list-find-position idn lista-idn)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable env idn))))

      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position idn proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (buscar-variable old-env idn))))
      
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
;PUNTO 2
;PASO 1: Defina un ambiente inicial con las variables @a @b @c @d @e con los valores (1 2 3 "hola" "FLP")
;PASO 2: modifique su funcion eval-expression para que acepte dicho ambiente.
;PASO 3: Diseñe una funcion llamada buscar-variable que recibe un simbolo (identificador) y un ambiente
;Retorna el valor si encuentra la variable en el ambiente, en el caso contrario "Error, la variable no existe"

;Pruebas:
; ->@a
; 1
; ->@b
; 2
; ->@e
; "FLP"

;PASO 1
(define inicial-env
  (lambda ()
    (extend-env '(@a @b @c @d @e) '(1 2 3 "hola" "FLP") (empty-env))))

;******************************************************************************************
;EJERCICIOS

;Ejercicio a
;Escriba un programa en su lenguaje de programación que contenga un procedimiento areaCirculo que permita calcular
;el area de un circulo dado un radio (A=PI*r*r). Debe incluir valores flotantes en su lenguaje de programación.
;Deberá invocarlo utilizando una variable @radio como parámetro:

; declarar(
;          @radio=2.5;
;          @areaCirculo=procedimiento (@insertarRadio) haga (3.14159 * (@insertarRadio * @insertarRadio)) finProc;
;          ){
;            evaluar @areaCirculo(@radio) finEval
;            }

;Ejercicio b
;Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular el
;factorial de un número n. Como la gramática para funciones recursivas debe ser propuesta por el grupo, incluya
;dos ejemplos de uso para el factorial de 5 y el factorial de 10.

; declarar (
; @f=procedimiento (@n) haga declaracion-rec
; @fact(@n) = Si @n entonces (@n * evaluar @fact(sub1(@n)) finEval ) sino 1 finSI {
; evaluar @fact (@n) finEval
; } finProc;){evaluar @f (10) finEval}

; declarar (
; @f=procedimiento (@n) haga declaracion-rec
; @fact(@n) = Si @n entonces (@n * evaluar @fact(sub1(@n)) finEval ) sino 1 finSI {
; evaluar @fact (@n) finEval
; } finProc;){evaluar @f (5) finEval}

;Ejercicio c
;Escriba un programa en su lenguaje de programación que contenga un procedimiento que permita calcular una
;suma de forma recursiva. Debe hacer uso de las funciones add1 y sub1 (remitase a la clase donde se implementó)
;la interfaz con las funciones zero, isZero?, sucessor, predecessor). Si no se evidencia el uso de add1 y sub1,
;el ejercicio no será valido. Incluya un llamado a la función recursiva: "evaluar @sumar (4, 5) finEval"

; declarar (
; @suma=procedimiento (@n,@n1) haga declaracion-rec
; @s(@n) = Si @n entonces  add1(evaluar @s(sub1(@n)) finEval )sino @n1 finSI {
; evaluar @s (@n,@n1) finEval
; } finProc;){evaluar @suma (9,2) finEval}

;Ejercicio d
;Escriba un programa en su lenguaje de programación que permita restar y multiplicar dos números haciendo uso solamente
;de las primitivas add1 y sub1. Incluya llamados:  "evaluar @restar (10, 3) finEval  ",  "evaluar @multiplicar (10, 3) finEval  ".

; declarar (
; 
; @resta=procedimiento (@x,@y) haga declaracion-rec
; @r(@y) = Si @y entonces  sub1(evaluar @r(sub1(@y)) finEval )sino @x finSI {
; evaluar @r (@y,@x) finEval
; } finProc;
; 
; 
; @multiplicar=procedimiento (@x,@y) haga declaracion-rec
; @m(@x) = Si @x entonces
;             Si @y entonces
; 
;                declarar (
;                @suma=procedimiento (@n,@n1) haga declaracion-rec
;                @s(@n) = Si @n entonces  add1(evaluar @s(sub1(@n)) finEval )sino @n1 finSI {
;                evaluar @s (@n,@n1) finEval
;                } finProc;){evaluar @suma ( evaluar @m(sub1(@x)) finEval,@y) finEval}
;                
;             sino 0 finSI
;          sino 0 finSI {
; evaluar @m (@x,@y) finEval
; } finProc;
; 
; ){evaluar @resta (10,5) finEval}

;Ejercicio e
;En python se puede utilizar algo que se llaman decoradores (por favor leer aquí). Crea una función @integrantes que muestre
;los nombres de los integrantes del grupo y adicionalmente crea un decorador que al invocarlo salude a los integrantes:

; declarar
; (
; @integrantes=procedimiento () haga "Cesar-Johan-Laura-Juan-Sheilly" finProc;
; @saludar=procedimiento (@entrada) haga ("Hola: " concat @entrada) finProc;
; )
; {
; declarar
; (
; @decorate=evaluar @saludar(evaluar @integrantes() finEval) finEval;
; )
; {
; @decorate
; }
; }

;Ejercicio f
;Modifique el ejercicio anterior para que el decorador reciba como parámetro otro mensaje que debe ponerse al final de
;todo el string (cualquier implementación sin el concepto de decorador no será evaluada).

; declarar
; (
; @integrantes=procedimiento () haga "Cesar-Johan-Laura-Juan-Sheilly" finProc;
; @saludar=procedimiento (@entrada) haga ("Hola: " concat @entrada) finProc;
; )
; {
; declarar
; (
; @decorate=procedimiento (@mensaje) haga (evaluar @saludar(evaluar @integrantes() finEval) finEval concat @mensaje) finProc;
; )
; {
; evaluar @decorate("-integrantes del grupo") finEval
; }
; }
