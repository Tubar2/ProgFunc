# Lista de Exercícios 5 - Applicative


# Parte 1

As soluções para esta parte da lista devem ser fornecidas no arquivo `AParser.hs`.

Um _parser_ é um algoritmo que recebe dados não-estruturados e produz dados estruturados como saída. Por exemplo, quando você carrega um arquivo-fonte no `ghci`, a primerira tarefa do interpretados é parsear seu texto para transformá-lo em uma _árvore sintática abstrata_ que representa seu código em uma forma estruturada. Nesta tarefa vamos representar um parser para um tipo `a` como uma função que recebe uma `String` representando a entrada a ser processada. Caso a função seja bem sucedida ela retorna o valor do tipo `a` junto do restante da `String` não processada.

```haskell
newtype Parser a = P (String -> Maybe (a, String))

runParser :: Parser a -> String -> Maybe (a, String)
runParser (P f) f
```

O exemplo a seguir define uma função `satisfy` que recebe um predicado sobre `Bool`s e constrói um parser que é bem sucedido se e somente se um `Char` satisfaz o predicado. Se ele encontra um `Char` que não satisfaz o predicado o parser falha.

```haskell
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P f
  where
    f [] = Nothing    -- fail on an empty input
    f (x:xs)
      | p x = Just (x, xs)    -- check if x satisfies the predicate
                              -- if so, return x along with the remainder
                              -- of the input (that is, xs)
      | otherwise = Nothing   -- otherwise, fail
```

A partir dessa função podemos definir um parser `char` que espera por um caracter específico.

```haskell
char :: Char -> Parser Char
char c = satisfy (== c)

*Parser> runParser (satisfy isUpper) "ABC"
Just (’A’,"BC")

*Parser> runParser (satisfy isUpper) "abc"
Nothing

*Parser> runParser (char ’x’) "xyz"
Just (’x’,"yz")
```

Por conveniência, também forneci um parser para os inteiros positivos.

```haskell
posInt :: Parser Integer
posInt = P f
  where f xs
          | null ns = Nothing
          | otherwise = Just (read ns, rest)
          where (ns, rest) = span isDigit xs
```

Implementar parser dessa forma, entretanto, é tedioso e sujeito a erros. O verdadeiro poder dessa abordagem surge da possibilidade de combinar parser simples para formar parser mais complexos. Esse poder vem do uso de `Applicative`!


## Item 1

Implemente a instância `Functor` para o tipo `Parser`.


## Item 2

Implemente a instância `Applicative` para o tipo `Parser`:

* `pure` representa o parser que não consome nenhum caracter da `String` de entrada e retorna um resultado do tipo `a`;

* `p1 <*> p2` representa o parser que primeiro executa o parser `p1` (que vai consumir dados e produzir a função) e então passa o que não foi consumido da `String` de entrada para o parser `p2` (que consome mais caracteres e produz algum valor). O resultado de `p1 <*> p2` é o resultado de aplicar a função que `p1` produziu no valor produzido por `p2`. Se qualquer um dos parsers falhar, `p1 <*> p2` também deve falhar.

Como exemplo de uso, suponha um tipo `Employee` definido como

```haskell
type Name = String
data Employee = Emp { name :: Name, phone :: string }
```

Se tivermos um `Name` e uma `String` podemos usar o construtor `Emp` para gerar um valor do tipo `Employee`. Podemos usar a instância `Applicative` de `Parser` para construir um parser para `Employee`s:

```haskell
parseName :: Parser Name
parsePhone :: Parser String
```

Com isso `Emp <$> parseName <*> parsePhone :: Parser Employee` é um parser que primeiro lê um nome da `String` de entrada, depois lê um telefone e combina tudo num valor do tipo `Employee`. Aqui assumimos que o nome e o telefone estão concatenados na `String` de entrada, sem separadores. Nos itens que seguem você vai implementar parsers capazes de "jogar fora" algumas informações durante a execução do parser.


## Item 3

Implemente os parser descritos a seguir utilizando a interface `Applicative`, não use a definição de baixo nível do tipo `Parser`.

* Construa o parser `abParser :: Parser (Char, Char)` que espera encontrar os caracteres `'a'` e `'b'` e os retorna em um par.

```haskell
*AParser> runParser abParser "abcdef"
Just ((’a’,’b’),"cdef")

*AParser> runParser abParser "aebcdf"
Nothing
```

* Construa o parser `abParser_ :: Parser ()` que funciona da mesma forma que `abParser`, mas retorna `()` como resultado ao invés do par contendo os caracteres `'a'` e `'b'`.

```haskell
*AParser> runParser abParser_ "abcdef"
Just ((),"cdef")

*AParser> runParser abParser_ "aebcdf"
Nothing
```

* Construa o parser `intPair` que lê dois valores inteiros separados por espaço e retorna os valores em uma lista. Utilize a função `posInt` fornecida.

```haskell
*Parser> runParser intPair "12 34"
Just ([12,34],"")
```


## Item 4

`Applicative` é suficiente para produzir parser para formatos simples e fixos, mas para formatos que envolvem algum tipo de escolha ele não é forte o bastante. Para lidar com escolhas, utilizamos a classe `Alternative` descrita resumidamente a seguir:

```haskell
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
```

`f1 <|> f2` representa uma escolha entre `f1` e `f2`. `empty` deve ser a identidade de `<|>` e comumente representa falhas.

Implemente a instância `Alternative` para o tipo `Parser` em que:

* `empty` representa o parser que sempre falha;

* `p1 <|> p2` representa o parser que tenta primeiro executar `p1`. Se `p1` é bem sucedido, `p2` é ignorado e o resultado de `p1` é utilizado. Caso contrário, se `p1` falhar, então `p2` é executado.

Para resolver esse item você precisa se preocupar com detalhes de baixo nível do tipo `Parser`.


## Item 5

Implemente um parser `intOrUppercase :: Parser ()` que procura por um inteiro ou por um caractere maiúsculo. Sua solução deve utilizar as interfaces providas por `Applicative` e por `Alternative`.

```haskell
*Parser> runParser intOrUppercase "342abcd"
Just ((), "abcd")

*Parser> runParser intOrUppercase "XYZ"
Just ((), "YZ")

*Parser> runParser intOrUppercase "foo"
Nothing
```

# Parte 2

As soluções para esta parte da lista devem ser fornecidas no arquivo `SExpr.hs`. Nesta parte utilize somente as interfaces `Functor`, `Applicative` e `Alternative` para construir o que se pede, não escreva suas funções a partir da definição de baixo nível do tipo `Parser`.


## Item 6

Vamos ver como implementar um parser para listas de objetos. Em particular, implemente duas funções:

* `zerOrMore :: Parser a -> Parser [a]` que recebe um parser como entrada e o executa quantas vezes forem possíveis, que pode ser nenhuma caso o parser falhe na primeira tentativa, e retorna uma lista de valores (potencialmente uma lista vazia);

* `oneOrMore :: Parser a -> Parser [a]` que funciona de maneira similar a anterior, mas que requer que o parser seja bem sucedido pelo menos uma vez. Se o parser falhar na primeira tentativa, então `oneOrMore` também falha.

Nos exemplos a seguir, usamos essas funções para construir parsers para sequências de caracteres maiúsculos.

```haskell
*AParser> runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")

*AParser> runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
Just ("ABC","dEfgH")
```

A diferença entre os dois pode ser vista quando não temos caracteres maiúsculos para consumir na `String` de entrada.

```haskell
*AParser> runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
Just ("","abcdeFGh")

*AParser> runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
Nothing
```


## Item 7

Implemente os seguintes parsers:

* `spaces :: Parser String` que consome uma lista de zero ou mais espaços em branco. Utilize a função `isSpace` do módulo `Data.Char` para facilitar sua implementação.

* `ident :: Parser String` que consome um _identificador_ composto por uma caractere alfabético (`isAlpha`) seguido de zero ou mais caracteres alfanuméricos (`isAlphaNum`). Em outras palavras, um identificador é uma sequência não vazia de letras e dígitos que não pode iniciar com um número.

```haskell
*AParser> runParser ident "foobar baz"
Just ("foobar"," baz")

*AParser> runParser ident "foo33fA"
Just ("foo33fA","")

*AParser> runParser ident "2bad"
Nothing

*AParser> runParser ident ""
Nothing
```

## Item 8

Uma expressão S é uma forma sintática para representar dados estruturados em árvores, originalmente utilizado na implementação da linguagem _LISP_. O último exercício pede que você implemente um parser para esse tipo de expressão.

Um identificador é representado por uma `String`, o formato é aquele do parser `ident` do item anterior.

```haskell
type Ident = String
```

Um _átomo_ pode ser um valor inteiro ou um identificador.

```haskell
data Atom = N Integer
          | I Ident
  deriving (Show)
```

Finalmente, uma expressão S é um átomo ou uma lista de outras expressões S.

```haskell
data SExpr = A Atom
           | Comb [SExpr]
  deriving (Show)
```

Opcionalmente, expressões S podem começar e terminar com qualquer número de espaços. Após desconsiderar os espaços antes e depois das expressões, elas podem ou não ser colocadas entre parênteses. Por exemplo, todas as expressão s a seguir são válidas:

| Input | Output |
|-------|--------|
| 5                                        | A (N 5) |
| foo3                                     | A (I "foo3")
| (bar (foo) 3 5 874)                      | Comb [A (I "bar"),Comb [A (I "foo")],A (N 3),A (N 5),A (N 874)] |
| (((lambda x (lambda y (plus x y))) 3) 5) | Comb [Comb [Comb [A (I "lambda"),A (I "x"),Comb [A (I "lambda"),A (I "y"),Comb [A (I "plus"),A (I "x"),A (I "y")]]],A (N 3)],A (N 5)] |
| ( lots of ( spaces in ) this ( one ) )   | Comb [A (I "lots"),A (I "of"),Comb [A (I "spaces"),A (I "in")],A (I "this"),Comb [A (I "one")]] |

O arquivo `SExpr.hs` fornece um tipo para representar expressões S. Escreva um parser `parseSExpr :: Parser SExpr` para expressões S.

**Dicas**

Para consumir dados da `String` de entrada e ignorar o resultado na saída utilize os operadores

```haskell
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a
```

`p1 *> p2` executa os parsers `p1` e `p2` em sequência, mas ignora o resultado de `p1`. `p1 <* p2` executa os parsers `p1` e `p2` em sequência, mas ignora o resultado de `p2`. Por exemplo:

```haskell
*AParser> runParser (spaces *> posInt) " 345"
Just (345,"")
```
