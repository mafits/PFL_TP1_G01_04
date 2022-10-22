# PFL_TP1_G01_04

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

> O primeiro passo na implentação foi criar uma função que permitisse comparar monómios e perceber se estes tinham a mesma parte literal -> *isEqual*

> O passo seguinte foi criar uma função que permisse a soma entre vários monómios com parte literal igual, retornando apenas o monómio final -> *joinMonList*

> De seguida, aplicando as funções anteriores, criámos uma função que soma todos os monómios com parte literal igual dentro do polinómio -> *joinPoly*

> Para removermos os casos em que o coefiente de um monómio é zero, ou a lista de coefiecientes está vazia, criamos também uma função -> *removeMon*

> De igual modo, criámos também uma função para remover da lista das incógnitas aquelas que tivessem expoente 0 -> *removeVar*

> Para completar a normalização criámos duas funções de ordenação:
- Para ordenar as incógnitas dentro de um monómio por ordem decrescente dos seus expoentes
- Para ordenar os monómios dentro de um polinómio por ordem decrescente do seu expoente máximo

> Assim, a função final da normalização aplica as duas funções de ordenação e as duas funções imediatamente antes mencionadas.

> Para imprimir na forma de String, chamámos uma função à parte -> `stringifyNormal` já que a função normal é utilizada com a representação interna noutras funções.

### Soma

>Para a soma, já que na normalização soma-se monómios com parte literal igual dentro de um polinómio, a nossa estratégia consistiu em juntar  as duas listas numa só, trantando-a como um só polinómio, e aplicar a função `normal.` A função que faz a soma de dois polinómios é `sumPoly`

>A nossa solução incluí também uma função que permite somar vários polinómios numa lista -> `sumPolyList`

> Ambas as funções retornam o resultado já na forma de *String*


