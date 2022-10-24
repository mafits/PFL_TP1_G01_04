# PFL_TP1_G01_04


Mafalda Bastos da Costa - up202006417

Sara Moreira Reis - up202005388

## Representação interna

### Monómio:

``` haskell 
([("x",1),("y",3)],[3])
```
    3*x*y^3 

### Polinómio:

``` haskell 
[([("x",1),("y",3)],[3]), ([("z",1),("y",3)],[-2]) ]
```
    3*x*y^3 - 2*z*y^3


> A primeira parte do túpulo representa a parte das incógnitas de um monómio. Está representado numa lista pois podem existir várias incógnitas dentro de um monómio. Cada incógnita é também representado por um túpulo, sendo a primeira parte a variável (na forma de String) e a segunda o seu expoente (na forma de um inteiro) `("x",2)` -> `x^2`

> A segunda parte do túpulo representa os coeficientes do monómio. Também é reprentado numa lista de inteiros, pois permitiu-nos comprimir todos os monómios com a mesma parte literal num só e assim facilitar certas operações tais como a soma.

Exemplo:
### Monómios com a mesma parte literal:

``` haskell 
([("x",1),("y",3)],[2,3])
```
    3*x*y^3 + 2*x*y^3

Uma das razões que nos motivou a escolher este padrão foi a necessidade de representar coeficientes e expoentes na forma de Inteiros (que nos facilitou na aplicação das operações básicas tais como somar, subtrair e multiplicar). Por este motivo, necessitámos de reprentar cada elemento da parte literal em forma de túpulos (já que as incógnitas e os seus respetivos expoentes tinham tipos diferentes), que, por sua vez, levou que o monómio também tivesse de ser reprentado por um túpulo, já que a parte literal é uma lista de Túpulos e os coeficientes uma lista de inteiros.

## Estratégia de Implementação

### Normalização

> O primeiro passo na implentação foi criar uma função que permitisse comparar monómios e perceber se estes tinham a mesma parte literal -> `isEqual`

> O passo seguinte foi criar uma função que permisse a soma entre vários monómios com parte literal igual, retornando apenas o monómio final -> `joinMonList`

> De seguida, aplicando as funções anteriores, criámos uma função que soma todos os monómios com parte literal igual dentro do polinómio -> `joinPoly`

> Para removermos os casos em que o coefiente de um monómio é zero, ou a lista de coefiecientes está vazia, criamos também uma função -> `removeMon`

> Para lidarmos com as incógnitas, criamos a função `handleVar`. Além de remover da lista das incógnitas aquelas que tivessem expoente 0 (`removeVar`), também soma os expoentes de incógnitas com a mesma variável dentro do mesmo monómio(`addEqualVar`). Esta última, verifica se na listagem de incógnitas de um monómio existem duas com variáveis iguais. Para testar todos os pares, verifica a igualdade de uma incógnita com todas as seguintes. No caso de terem variável igual, soma o expoente da segunda ao expoente da primeira, ficando apenas a primeira incógnita com o expoente "atualizado", sendo a segunda eliminada.

> Para completar a normalização criámos duas funções de ordenação:
- Para ordenar as incógnitas dentro de um monómio por ordem decrescente dos seus expoentes; Quando estes são iguais ordenámos as incógnitas por ordem alfabética.
- Para ordenar os monómios dentro de um polinómio por ordem decrescente do seu expoente máximo

> Assim, a função final da normalização aplica as funções acima mencionadas.

> A função final da normalização é `normalString` que recebe e imprime na forma de String -> `normalString ::  String -> String`


### Soma

>Para a soma, já que na normalização soma-se monómios com parte literal igual dentro de um polinómio, a nossa estratégia consistiu em juntar  as duas listas numa só, trantando-a como um só polinómio, e aplicar a função `normal.` A função que faz a soma de dois polinómios é `sumString`

> A nossa solução incluí também uma função que permite somar vários polinómios numa lista -> `sumListString`

> Ambas as funções recebem o input e retornam o resultado já na forma de *String*


### Multiplicação

> O primeiro passo foi criar uma função que multiplicasse dois monómios, ou seja, que concatenasse as duas partes literais e multiplicasse os seus coeficientes -> `mulMon`

> De seguida, aplicou-se a função anterior de forma distributiva. Para tal, criámos outra função que multiplicasse um monómio de um dos polinómios por todos os outros monómios do outro polinómio -> `mulMonPoly`

> Por último, aplicámos a função de cima a todos os monómios do primeiro polinómio, multiplicando assim todas as componentes de ambos os polinómios -> `mulPoly`

> Após termos o polinómio final, procedemos à sua normalização, para juntar incógnitas dentro de um monómio com igual variável e somar os coeficientes monómios com iguais incógnitas -> `mul`

> A função final (`mulString`) recebe e retorna o resutado na forma de String


### Derivação

> Derivamos monómio e a monómio.

> Testamos se um monómio tem a variável que pretendemos derivar e, no caso, procedemos à derivação. No caso contrário, devolvemos um monómio com coeficiente nulo.

> Tendo um monómio a variável a derivar, derivamos as incógnitas que têm a variável, uma a uma (multiplicamos o coeficiente pelo expoente da incógnita e subtraímos o expoente por 1), e concatenamos com as que não têm.

> Normalizamos o polinómio final para retirar coeficientes nulos, incógnitas com expoente nulo e juntar monómios com incógnitas iguais.


### Imprimir na forma de String

> Numa primeira fase criámos uma função que transformasse as incógnitas de um monómio e o seu respetivo expoente numa string -> `stringifyVar`

> De seguida, utilizámos a função acima para criar outra que passasse para string um monómio completo -> `stringifyMon`

> Aplicando a mesma lógica, criámos outra função que utilizasse a anterior em todos os monómios de um polinómio e acrescentasse os sinais de operações entre os mesmos ->  `stringifyPol`

> Por fim, o passo final foi retirar o sinal '+' do primeiro monómio se este fosse positivo -> `stringify`
<br>

### Parsing (de String para a representação interna)

> Aplicamos uma função para remover os espaços todos na String introduzida -> `removeSpace`

> Na função `parsePoly` dividimos o polinómio em monómios, a partir da deteção de um sinal de adição ('+') ou de subtração ('-').

> Para o parsing de cada monómio, dividimos o mesmo nas suas componentes. Isto é, representamos os seus coeficientes e incógnitas numa lista de String, a partir da detação do sinal de multiplicação ('\*'). Por exemplo, "3\*x^2\*y" passa a ser representado por ["3", "x^2", "y"] -> `divideInComp`

> Filtramos as componentes que têm uma letra, logo, que são uma incógnita (`hasLetter`), e colocamo-las no formato que pretendido: par (variável, expoente). Por exemplo, "x^2" passa a ser ("x",2). -> `parseVar`

> As componentes que não têm letra, são coeficientes. Neste caso, só transformamos a String que as representa em inteiro. -> `parseCoef`

> Finalmente, juntamos estes dois últimos parsings no formato de monómio pretendido. "3\*x^2\*y" passa a ([("x",2),("y",1)],[3]). -> `parseMon`

## Exemplos de Teste

<br>

### Normalização

``` haskell 
normalString "3*x*y^3 + 5*x*y^3 - 9*x^7 - 7*y^3*x" 
```
> Output: "- 9*x^7 + 1*xy^3"
``` haskell 
normalString "9   *  z^9 + x*  y^3 - 0*x - 7*y^3*x"
```
> Output: "9*z^9 - 6*xy^3"
``` haskell 
normalString "" 
```
> Output: ""
``` haskell 
normalString "9*z^0 - 3*a^3 +2*x" 
```
> Output: "- 3*a^3 + 2*x + 9"
``` haskell 
normalString  "2*x^2*x*x"
```
> Output: "2*x^4"
``` haskell 
normalString  "2*x^0*y^1"
```
> Output: "2*y"

<br>

### Soma
``` haskell 
sumString "- 2*x^2 + 3*y^5 -x"  "- 2*x^2 + 0*y^5 -3*x"
```
> Output: "3*y^5 - 4*x^2 - 4*x"
``` haskell 
sumString "- 2*x^2 + 3*y^5 -x"  "- 2*x^2 + 3*y^5 -x"
```
> Output: "6*y^5 - 4*x^2 - 2*x"
``` haskell 
sumString "- 2  *   x^  2 +  3*  y ^ 5  - 2 * x"  ""
```
> Output: "3*y^5 - 2*x^2 - 2*x"
``` haskell 
sumListString ["- 2*x^2 + 3*y^5 -x", "- 2*x^2 + 3*y^5", "3*x"] 
```
> Output: "6*y^5 - 4*x^2 + 2*x""

<br>

### Multiplicação
``` haskell 
mulString "- 2*x^2 + 3*y^5 -x"   "- 2*x^2 + 3*y^5"
```
> Output: "9*y^10 - 3*xy^5 - 12*x^2y^5 + 4*x^4 + 2*x^3"
``` haskell 
mulString "- 2*x^2 + 3*y^5 -x"   "0" 
```
> Output: "0"
``` haskell 
mulString "- 2  *  x  ^  2 + 3*  y  ^  5 -  3 * x  "   "x^0"
```
> Output: "3*y^5 - 2*x^2 - 3*x"
``` haskell 
mulString  "7*x^4+2*y" "0*x*y + 2"
```
> Output: "14*x^4 + 4*y"

<br>

### Derivação
``` haskell 
deriveString  "x^4 + 2*y*x^6 + 5*z^3"  "x"
```
> Output: "12*yx^5 + 4*x^3"
``` haskell 
deriveString "3*z + 1"  "x" 
```
> Output: "0"
``` haskell 
deriveString   "x ^ 2 +3 * z -4 * z   ^2 "  "z"
```
> Output: "- 8*z + 3"
``` haskell 
deriveString   "x +0*x"  "x"
```
> Output: "1"

