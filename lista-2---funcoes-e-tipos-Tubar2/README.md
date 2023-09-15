# Lista de Exercícios 2

# Item 1:

A linguagem VHDL especifica a biblioteca `stdlogic` que define os
valores lógicos e suas operações básicas. Trata-se de uma extensão da
álgebra booleana que além dos valor `0` (Falso) e `1` (Verdadeiro),
adiciona os valores `Z` (Alta impedância), `W` (sinal fraco, não é
possível determinar se é verdadeiro ou falso), `L` (`0` fraco), `H`
(`1` fraco), `-` (don't care), `U` (não-inicializado/indefinido) e `X`
(desconhecido/conflito de fontes).

Vamos implementar em Haskell uma versão simplificada dessa lógica no
arquivo `StdLogic.hs` (um esqueleto da implementação foi
fornecido).

No diretório raíz do seu repositório, execute `stack test` para
compilar e executar os casos de teste já fornecidos. A sua
implementação precisa passar em todos os casos de teste além de outros
que serão executados durante a correção. Dica, os casos de teste
funcionam como parte da especificação para suas funções.

1. O arquivo `StdLogic.hs` contém um tipo `StdLogic` cujos valores
   possíveis são `T` (True), `F` (False), `X` (desconhecido) e `Z`
   (alta impedância).

2. Defina instâncias para as classes `Eq` e `Show` para que seu tipo
   possa ser utilizado para testes. Considere que

```haskell
class Show a where
  show :: a -> String

class Eq a where
  (==) :: a -> a -> Bool
```

3. Defina as funções lógicas descritas pelas tabelas a seguir. A
   primeira linha indica as entradas da função. Para funções com duas
   entradas, a última coluna indica a segunda entrada.

* `stdNot :: StdLogic -> StdLogic`

| X | F | T | Z |
|---|---|---|---|
| X | T | F | X |


* `stdAnd :: StdLogic -> StdLogic -> StdLogic`

| X | F | T | Z |       |
|---|---|---|---|-------|
| X | F | X | X | **X** |
| F | F | F | F | **F** |
| X | F | T | X | **T** |
| X | F | X | X | **Z** |


* `stdOr :: StdLogic -> StdLogic -> StdLogic`

| X | F | T | Z |       |
|---|---|---|---|-------|
| X | X | T | X | **X** |
| X | F | T | X | **F** |
| T | T | T | T | **T** |
| X | X | T | X | **Z** |


* `stdXor :: StdLogic -> StdLogic -> StdLogic`

| X | F | T | Z |       |
|---|---|---|---|-------|
| X | X | X | X | **X** |
| X | F | T | X | **F** |
| X | T | F | X | **T** |
| X | X | X | X | **Z** |


4. Defina uma função `adder b1 b2 carryIn` que modela um somador
   completo de 1 bit.

5. Defina uma função `mux b1 b2 select` que modela um multiplexador
   2x1.

# Item 2

Nesta parte você irá implementar uma pequena biblioteca para modelar
números complexos. Números complexos podem ser compreendidos como
pontos em umn plano bidimensional, representados por duas coordenadas
reais. Complete o arquivo `Complex.hs` (um esqueleto da
implementação foi fornecido).

No diretório raíz do seu repositório, execute `stack test`
para compilar e executar os casos de teste já fornecidos. A sua
implementação precisa passar em todos os casos de teste além de outros
que serão executados durante a correção.

Dica 1: os casos de teste funcionam como parte da especificação para
suas funções.

Dica 2: despreze erros de arredondamento que aconteçam com operações
de ponto flutuante.

1. Implemente uma função `isRect :: Complex -> Bool` e uma função
   `isPolar :: Complex -> Bool` que retornam `True` caso um número
   arbitrário `c :: Complex` esteja representado em coordenadas
   retangulares ou polares, respectivamente.

2. Implemente uma função `toRect :: Complex -> Complex` e uma função
   `toPolar :: Complex -> Complex` que alternam entre as
   representações retangular e polar.

3. Implemente instâncias paras as classes `Show` e `Num`. Para a
   classe `Num`, `abs c` deve retornar um número complexo com a
   magnitude de `c` e orientado na direção real positiva; `signum c`
   retorna um número com a mesma fase de `c`, mas magnitude unitária;
   `negate c` retorna o conjugado de `c`, e `fromInteger n` recebe um
   inteiro e deve retornar um número complexo na forma retangular com
   a parte real igual a `n`.

Dica 3: você provavelmente vai precisar implementar outras funções
auxiliares para conseguir implementar o que foi pedido.
