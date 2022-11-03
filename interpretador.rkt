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

;especificacion lexica 

(define scaner-spec
'(
    (white-sp (whitespace) skip)
    (comment ("//" (arbno (not #\newline))) skip)
    (identificador 
    ("@" (arbno (or letter digit)) "?") symbol)
    (texto("/" (arbno (or letter digit)) "/") string)
    ; enteros positivos y negativos
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    ; flotantes positivos y negativos
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)
)
)

;especificacion sintactica 

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numero) numero-lit)
    (expresion (texto) texto-lit)
    (expression (identificador) var-exp)
    (expression (primitiva-unaria "("expression")") primapp-un-exp)
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    ))
;Especificación Léxica

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip)
  (comment
    ("%"(arbno (not #\newline))) skip)
  ;pregunta como colocar \ \ y letras y numeros al tiempo
  (texto("/" (arbno (or letter digit)) "/") string)
  ;pregunta solo debe ser valido un ? y cómo se haría
  (identificador
   ("@"
    (arbno (or letter digit)) "?") symbol)
  ; enteros positivos y negativos
  (numero (digit (arbno digit)) number)
  (numero ("-" digit (arbno digit)) number)
  ; flotantes positivos y negativos
  (numero (digit (arbno digit) "." digit (arbno digit)) number)
  (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (numero) numero-lit)
    (expression (texto) texto-lit)
    (expression (identificador) var-exp)
    (expression (primitiva-unaria "("expression")") primapp-un-exp)
    ;;;;;;
    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
  ; características adicionales
    (expression ("Si" expression "entonces" expression "sino" expression "finSI")
                condicional-exp)
    (expression ("let" (arbno identificador "=" expression) "in" expression)
                let-exp))
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
  (sllgen:make-rep-loop  "--> "
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
     '(@x @y @z)
     '(4 2 5)
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (apply-env env id))
      (condicional-exp (test-exp true-exp false-exp)
              (if (true-value? (eval-expression test-exp env))
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (primapp-un-exp (prim-unaria exp)
                      (apply-un-primitive prim-unaria (eval-expression exp env)))
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

;apply-primitive: <primitiva> <list-of-expression> -> numero
(define apply-un-primitive
  (lambda (prim args)
    (cases primitiva-unaria prim
      (primitiva-longitud ()(-(string-length args)2))
      (primitiva-add1 () (+ args 1))
      (primitiva-sub1 () (- args 1)))))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*******************************************************************************************
;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))

                       (env environment?)))

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
                                 (apply-env env sym)))))))


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
    






#|
    2)Defina un ambiente inicial con las variables (@a @b @c @d @e) con valores (1 2 3 "hola" "FLP")
    y modifique su función evaluar-expresión para que acepte dicho ambiente.
    -Diseñe una función llamada (buscar-variable) que recibe un símbolo (identificador) y un ambiente,
    retorna el valor si encuentra la variable en el ambiente. En caso contrario: "Error, la variable 
    no existe"
|#

#|
    Pruebe:
    --> @a
    1
    --> @b
    2
    --> @e
    "FLP"
|#

#|
    3) Implemente los Booleanos:
    En una expresión numérica, 0 es falso, cualquier otro caso es verdadero. Para esto diseñe la
    función valor-verdad? que realiza esta verificación.
|#

#|
    4) Extienda la gramática con condicionales:
    <expresion> := Si <expresion> entonces <expresion>  sino <expresion>
    finSI condicional-exp (test-exp true-exp false-exp)
|#

#|
    Debe probar:
    --> Si (2+3) entonces 2 sino 3 finSI
    2
    --> Si (longitud(@d) ~ 4) entonces 2 sino 3 finSI
    3
|#

#|
    5) Implemente declaración de variables locales:
    <expresion> := declarar (<identificador> = <expresion> (;)) { <expresion> }
                variableLocal-exp (ids exps cuerpo)
|#

#|
    Debe probar:
    --> declarar (@x=2;@y=3;@a=7){
        (@a+(@x~@y))
        }
    6
    --> declarar (@x=2;@y=3;@a=7) {
        (@a+@b)
        }
   9
|#



#|
    6) Extienda la gramática para crear procedimientos
    <expresion> := procedimiento (<identificador>*',') haga <expresion> finProc procedimiento-ex (ids cuero)
    Para esto debe definir un datatype para la cerradura (o ProcVal) que debe tener 3 campos:
    1. Lista ID del procedimiento
    2. Cuerpo del procedimiento
    3. Ambiente donde fue declarado|#

#|

(define-datatype procVal procVal?
  (cerradura
   (lista-ID (list-of symbol?))
   (exp expresion?)
   (amb ambiente?)
   )
)
|#

#|
    Debe probar:
    --> procedimiento (@x,@y,@z) haga ((@x+@y)+@z) finProc
    #(struct:cerradura (@x @y @z) #(struct:primapp-bin-exp #(struct:primapp-bin-exp #(struct:var-exp
    @x) #(struct:primitiva-sum) #(struct:var-exp @y)) #(struct:primitiva-sum) #(struct:var-exp @z)) #
    (struct:extendido (@a @b @c @d @e) (1 2 3 "hola" "FLP") #(struct:vacio)))
    Se debe retornar una cerradura
|#

#|
    7) Extienda la gramática para evaluar procedimientos:
    <expresion> :=  "evaluar" expresion   (expresion ",")*  finEval
                app-exp(exp exps)
 |#

 #|
    Debe probar:
    -->  declarar (
         @x=2;
         @y=3;
         @a=procedimiento (@x,@y,@z) haga ((@x+@y)+@z) finProc
         ) {
         evaluar @a (1,2,@x) finEval
         }
    5
    --> declarar (
        @x=procedimiento (@a,@b) haga ((@a*@a) + (@b*@b)) finProc;
        @y=procedimiento (@x,@y) haga (@x+@y) finProc
        ) {
        ( evaluar @x(1,2) finEval + evaluar @y(2,3) finEval )
        }
        10
    --> declarar (
        @x= Si (@a*@b) entonces (@d concat @e) sino longitud((@d concat
        @e)) finSI;
        @y=procedimiento (@x,@y) haga (@x+@y) finProc
        ) {
        ( longitud(@x) * evaluar @y(2,3) finEval )
          }
    35
|#

;8) Extienda la gramática para incluir llamados recursivos. Proponga una definición en la gramática e impleméntela.

;9) Dibuje el árbol de sintaxis abstracta de los ejercicios del punto 7.

;10) Dibuje el diagrama de ambientes de los ejercicios del punto 7 (tenga en cuenta el ambiente inicial del punto 1).

;11) Utilización del lenguaje de programación:
