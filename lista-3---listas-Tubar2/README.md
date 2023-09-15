# Lista de Exercícios 3

Implemente todas as funções descritas a seguir no arquivo-fonte `src/Data/Functions.hs`.

O arquivo `src/Main.hs` fornece diversos casos de teste para a Parte 1 da lista a seguir. Para compilar e executar os testes já fornecidos, execute `stack test`.

# Parte 1

Forneça implementações para as funções descritas a seguir, mantendo as assinaturas de tipo. Se você encontrar uma função na biblioteca-padrão que faz a mesma coisa, não utilize a função equivalente, implemente sua própria versão (por exemplo, não faça `member = elem`).

Todas as funções a seguir podem ser implementadas em uma única linha, tente fazer isso (requisito não obrigatório, mas tente. A dica é fazer uma definição com pattern matching e depois tentar reescrever em termos das funções `map`, `foldr`, `foldr`, `filter`, etc). Nenhuma delas precisa funcionar para listas parciais/listas infinitas.

Veja os casos de teste para exemplos de uso.


1. `member :: Eq a => a -> [a] -> Bool`

Retorna verdadeiro se o elemento aparece na lista.

2. `count :: (Num a, Eq a1) => a1 -> [a1] -> a`

Conta o número de vezes que um elemento aparece na lista.

3. `forall :: (a -> Bool) -> [a] -> Bool`

Testa se todos os elementos da lista satisfazem uma determinada propriedade.

4. `exists :: (a -> Bool) -> [a] -> Bool`

Testa se algum elemento da lista satisfaz uma determinada propriedade.

5. `first :: (a -> Bool) -> [a] -> Maybe a`

Encontra o primeiro elemento da lista que satisfaz uma determinada propriedade.

6. `single :: (a -> Bool) -> [a] -> Bool`

Testa se somente um elemento da lista satisfaz uma determinada condição.

7. `mostly :: (a -> Bool) -> [a] -> Bool`

Verdadeiro se a lista tem mais elementos que satisfazem uma determinada propriedade do que elementos que não a satisfazem.

8. `mostlyTrue :: [Bool] -> Bool`

Verdadeiro se a lista contém mais `True`s do que `False`s (se igual, o resultado é `False`).


# Parte 2

As funções a seguir são mais desafiadoras e envolvem séries numéricas. Além das funções, forneça casos de teste para demonstrar a funcionalidade de cada função.

9. `majority :: Eq a => [a] -> Maybe a`

Encontre, em tempo linear, o elemento que aparece em mais do que a metade das posições da lista. Retorne `Nothing` se não houver um elemento de maioria. Veja https://www.cs.utexas.edu/~moore/best-ideas/mjrty/ para um algoritmo (o paper que descreve o algoritmo em detalhes é este: https://www.cs.ou.edu/~rlpage/dmtools/mjrty.pdf).

10. Uma famosa proposição que intriga os matemáticos até hoje é a conjectura de Collatz (https://en.wikipedia.org/wiki/Collatz_conjecture). Ela trata de uma sequência definida da seguinte maneira: comece com um número inteiro positivo `n`. O próximo termo da sequência é obtido a partir do anterior pela seguinte regra: se o termo anterior é par, o próximo termo é metade do anterior; se o termo anterior é ímpar o próximo termo é 3 vezes o anterior mais 1. A conjectura afirma que a sequência sempre atinge 1, não importa o valor inicial de `n`.

a) `collatz1 :: Int -> Int`

Executa um passo da sequência de Collatz. `collatz1 1 = 1`; `collatz n = n / 2` se `n` é par; `collatz n = 3 * n + 1`, se `n` é ímpar.

b) `collatz :: Int -> [Int]`

Constroi a sequência de Collatz começando de um número `n`. A sua lista deve finalizar sempre que a sequência alcançar o número 1, isto é, `last $ collatz n = 1`.

11. Um ponto fixo `x` de uma função `f` é um valor tal que `f x = x`. Perceba que a função `collatz` definida acima tem o valor `1` como ponto fixo.

a) `isFixpoint :: (Eq a) => (a -> a) -> a -> Bool`

`isFixPoint f x` retorna verdadeiro se `x` é ponto fixo da função `f`.

b) `findFixpoint :: (Eq a) => (a -> a) -> a -> Int -> Maybe a`

Em alguns casos, a aplicação repetida de uma função gera um ponto fixo, isto é, `f(f(f(... x ...))) = x`. `findFixPoint f x limit` tenta encontrar um ponto fixo de `f` começando por `x`. Se ela tiver sucesso, ela deve retornar `Just y`, onde `y` é o ponto fixo encontrado. Se após `limit` tentativas ela não tiver convergido, a função retorna `Nothing`.

c) `testCollatzConjecture :: Int -> [Int] -> [Bool]`

Se a conjectura de Collatz estiver correta, a função `findFixPoint` deverá ser capaz de sempre encontrar o ponto fixo da função `collatz1`. `testCollatzConjecture limit` recebe uma lista de `Int`s e testa se a conjecture de Collatz é verdadeira para cada elemento da lista utilizando `findFixpoint`.

12. A página da Wikipedia (https://en.wikipedia.org/wiki/Convergent_series) mostra exemplos de séries convergentes. Vamos escrever funções que permitam calcular estas séries.

a) `tooBig :: Double -> Bool`

Retorna `True` se o argumento fornecido é maior do que 0.0001.

b) `nearlyEqual :: Double -> Double -> Bool`

Retorna `True` se os valores fornecidos são próximos. Faça isso dividindo um número pelo outro e checando se o resultado está no intervalo entre 0.9999 e 1.0001. Atenção a possibilidade de divisão por zero!

c) `sequence :: (Int -> Double) -> Int -> [Double]`

Dada uma função `f` e um valor inicial `i`, retorna a lista infinita `[f(i), f(i+1), f(i+2), f(i+3), ...]`.

d) `series :: (Int -> Double) -> Int -> [Double]`

Dada uma função `f` e um valor inicial `i`, retorna a lista infinita `[f(i), f(i)+f(i+1), f(i)+f(i+1)+f(i+2), f(i)+f(i+1)+f(i+2)+f(i+3), ...]`.

<!-- HIDDEN TIP FOR SERIES -->
<!-- series' :: (Int -> Double) -> Int -> Double -->
<!-- series' f i = undefined -->

e) Escreva as funções `computeLn2`, `computeE`, `computePsi`. Você pode parar as somas quando qualquer dos termos da sequência se tornar menor do que 0.0001. Você pode utilizar a função `series`, mas talvez precise definir outras funções auxiliares.

Obs.: [Informação sobre Psi](https://en.wikipedia.org/wiki/Reciprocal_Fibonacci_constant)
