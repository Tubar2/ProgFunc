# Tipos - Classes

Nessa lista de exercícios, vamos ver maneiras diferentes de
implementar uma calculadora, assim como interagir com tipos de dados
que representam uma expressão aritmética.

O tipo `Expr` (definido em `src/Syntax.hs`) representa uma expressão
aritmética. O tipo é composto de um caso base `Num` que contém um
inteiro, e de um caso recursivo para cada operação (`+`, `-`, `*`,
`/`). Desse modo, podemos representar expressões de complexidade
arbitrária.


```haskell
data Expr = Num  Int
          | Add  Expr Expr
          | Sub  Expr Expr
          | Mult Expr Expr
          | Div  Expr Expr
```

Como exemplo, tomemos a expressão `2 + 3 * 5`. Podemos representá-la
com o tipo `Expr` da seguinte maneira:
`Add (Num 2) (Mult (Num 3) (Num 5))`.

Vejamos agora uma representação que, além de expressões involvendo
números, permite também que usemos *variáveis*.

```haskell
data Expr = Num  Int
          | Var  String
          | Add  Expr Expr
          | Sub  Expr Expr
          | Mult Expr Expr
          | Div  Expr Expr
```

Temos, portanto, para a expressão `2 + 3 * x`, a representação
`Add (Num 2) (Mult (Num 3) (Var "x"))`.

# Item 1

No arquivo `src/Interpreter.hs`, escreva a função `eval :: Expr -> Maybe Int`
que recebe um valor do tipo `Expr` e retorna o resultado da
avaliação da expressão correspondente.  Divisão por zero deve resultar
em `Nothing`.  Para este item, ignore definições de variáveis como
denotado pelo construtor `Var`.

# Item 2

No módulo `Parser`, temos uma função `parseString` que lê uma
expressão aritmética em formato de texto e faz o parsing dela para o
tipo `Expr`. Como esse processo pode resultar em erro, o valor
retornado é do tipo `Maybe Expr`.

Escreva a função `evalStr :: String -> Maybe Int` que faz o parsing da
string recebida e retorna o valor de sua avaliação, caso o parsing
seja bem sucedido, ou retorne `Nothing` caso contrário.  Como no item
anterior, casos de divisão por zero também devem retornar `Nothing`.

# Item 3

Pode ser conveniente obtermos uma representação de texto do tipo
`Expr` que corresponda com a forma na qual escrevemos expressões
aritméticas. Contudo, atualmente, se mostramos um valor `Expr` na tela,
obtemos a textual de acordo com o a árvore do tipo.

O módulo `Display` contém a definição de uma typeclass a ser utilizada
para esse propósito. A classe `Display` possui uma função `display`
que recebe um valor e retorna uma string que pode ser usada para
exibir o valor recebido. No arquivo `src/Syntax.hs`, implemente a
instância dessa classe para o tipo `Expr`.

```haskell
instance Display Expr where
  display = ???
```

Para a implementação da função `display`, considere que expressões
sempre contêm parenteses, para evitar ambiguidade e facilitar a
implementação.

Seguem alguns exemplos do uso da função `display`.

```haskell
display (Add (Num 1) (Num 2)) = "(1 + 2)"
display (Add (Num 1) (Add (Num 2) (Num 3))) = "(1 + (2 + 3))"
```

# Item 4

Apesar do sistema de avaliação da função `eval` funcionar como requisitado,
a travessia de uma árvore sintática pode ser custosa quando o número de nós
é muito grande. Interpretadores muitas vezes usam uma estrutura de bytecode,
que é uma sequência linear de instruções básicas.

O módulo `VM` contêm a definição de um bytecode básico de operações
de uma máquina de pilha. Podemos empilhar um valor inteiro com a
instrução `PUSH`. As operações `ADD`, `SUB`, `MULT`, e `DIV` operam
nos dois valores no topo da pilha, como demonstrado na ilustração.
`---` representa a base da pilha, e `+++` representa o topo.

```
+++
ADD
  1  --> +++
  2        3
---      ---
```

Para as operações não-comutativas `SUB` e `DIV`, considere a seguinte
ordem.

```
+++
SUB
  1  --> +++
  4        3
---      ---
```

Implemente a função `compile :: Expr -> [Bytecode]` que implementa um
transformação de uma expressão aritmética para uma sequência de
instruções para uma máquina de pilha.
Para este item, ignore definições de variáveis como denotado pelo
construtor `Var`.

# Item 5

Implemente a função `runBytecode :: [Bytecode] -> Maybe Int` que
executa uma sequência de instruções e, após finalizada, retorna o valor
no topo da pilha. Caso haja algum erro durante a execução, retorne `Nothing`.

Considere somente sequências bem formadas de instruções. Uma instrução
é bem formada se e somente se
- a cada ponto onde um operador é executado existem operandos
  suficientes na pilha, e
- Ao finalizada a sequência, existe exatamente um valor na pilha, e
  este é o resultado da avaliação.

# Item 6

Em algum ponto, é útil introduzir a funcionalidade de variáveis em
expressões. Para isso, podemos passar uma estrutura de ambiente
para o avaliador. Essa estrutura contém associações de identificadores
textuais (variáveis) a valores inteiros.

Implemente a função `evalV` que recebe um ambiente e uma expressão e
retorna o resultado da avaliação da expressão. Essa função será bem semelhante
à função `eval`, mas com suporte ao construtor `Var`.
Caso seja referenciada uma variável não definida no ambiente, o resultado
deve ser `Nothing`.
