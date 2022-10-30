#lang eopl

#|
    Diseñe un interpretador para la siguiente gramática que realiza
    operaciones con notación infija:
    Valores denotados: Texto + Número + Booleano + ProcVal
    Valores expresado: Texto + Número + Booleano + ProcVal
|#

#|
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
                primapp-un-exp (prim-unaria exp)|#

#|
    <primitiva-binaria> :=  + (primitiva-suma)
                        :=  ~ (primitiva-resta)
                        :=  / (primitiva-div)
                        :=  * (primitiva-multi)
                        :=  concat (primitiva-concat)
|#

#|
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
    función valor-verdad? que realiza esta verificación.|#

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
