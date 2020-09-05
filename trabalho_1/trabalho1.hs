-- 1) Escreva a função triangulo que, dados três valores de ângulos (em graus), verifique
-- se podem representar os ângulos internos um triângulo e retorne o tipo do triângulo
-- resultante (como uma string). Os retornos podem ser:
-- • “equilatero”: se os três ângulos são iguais
-- • “retangulo”: se um dos ângulos é 90 graus
-- • “obtuso”: se um dos ângulos é maior que 90 graus
-- • “simples”: se é um triângulo e não se enquadra em nenhum dos casos anteriores
-- (equilátero, retângulo ou obtuso)
-- • “nao_triangulo”: se não puder ser classificado como triângulo

triangulo::Double->Double->Double->String
triangulo a b c
  | (a+b+c) /= 180.0 || a <= 0 || b <= 0 || c <= 0 = "nao_triangulo"
  | a == b && b == c = "equilatero"
  | a > 90 || b > 90 || c > 90 = "obtuso"
  | a == 90 || b == 90 || c == 90 = "retangulo"
  | otherwise = "simples"

-- 2) Escreva a função equacao que recebe três valores reais a, b, c. Se a for diferente de
-- 0, a função retorna uma tupla com as duas raízes da equação de segundo grau ax2 + bx
-- + c = 0. Se Se a for igual a 0, a função retorna uma tupla, sendo o primeiro elemento a
-- solução da equação de primeiro grau bx + c = 0 e o segundo elemento o próprio a.

equacao::Double->Double->Double->(Double,Double)
equacao a b c
  | a == 0.0 = ((-c)/b, a)
  | otherwise = (raiz1, raiz2)
  where
      delta = b*b -4*a*c;
      raiz1 = (((-b) + (sqrt(delta)))/(2*a));
      raiz2 = (((-b) - (sqrt(delta)))/(2*a))

--3) Considere que o preço de uma passagem de ônibus intermunicipal pode variar
--dependendo da idade do passageiro. Crianças até 10 anos pagam 40% e bebês (abaixo
--de 2 anos) pagam apenas 15%. Pessoas com 70 anos ou mais pagam apenas 50% do
--preço total. Os demais passageiros, pagam a tarifa normal (100%). Faça uma função que
--tenha como entrada: o valor total da passagem, a data atual e a data de nascimento do
--passageiro. Como saída, a função retorna o valor a ser pago. (Obs.: na solução, deve ser
--definido o tipo data para representar a tupla (d,m,a)).

type Data=(Int, Int, Int)
calculaIdade::Data->Data->Int
calculaIdade (d_atual, m_atual, a_atual) (d_nasc, m_nasc, a_nasc)
  | m_atual > m_nasc = a_atual - a_nasc
  | m_atual < m_nasc = a_atual - a_nasc - 1
  | d_atual >= d_nasc = a_atual - a_nasc
  | otherwise = a_atual - a_nasc - 1

calculaPrecoPassagem::Double->Data->Data->Double
calculaPrecoPassagem preco dataAt dataNasc
  | idade < 2 = preco*0.15
  | idade < 10 = preco*0.4
  | idade >= 70 = preco*0.5
  | otherwise = preco
  where
    idade = calculaIdade dataAt dataNasc

--4) Construa funções que gerem as seguintes listas, utilizando-se lista por compressão.
--Todas as funções devem utilizar a lista de inteiros de 1 a 15 em pelo menos um dos
--geradores. Apresentar o código da função e o resultado da lista gerada.

--a) gera1: gerar a lista de inteiros, contendo o quadrado de
--todos os ímpares entre 4 e 14.
gera1::[Int]
gera1 = [x^2 | x <- [1..15], odd x, x > 4, x < 14]

--b) gera2: gerar a lista de duplas formadas tendo o primeiro
--elemento entre 1 e 4 e o segundo elemento no intervalo
--fechado entre o valor do primeiro elemento e o seu dobro.
gera2::[(Int, Int)]
gera2 = [ (x,y) | x <- [1..4], y <- [x .. 2*x]]

--c) gera3: a partir de uma lista l1 entre 10 e 15, gerar a
--lista com todos os elementos dentro do intervalo fechado
--definido entre 1 e cada elemento de l1 (Obs.: pode ter
--elemento repetido na lista final).
gera3::[Int]
gera3 = [ lista | l1 <-[10..15], lista <-[1..l1] ]

--d) gera4: gerar uma lista de duplas, onde cada dupla são 2
--números consecutivos de 1 a 16, sendo o primeiro elemento
--ímpar (Ex: (1,2) e (3,4))
gera4::[(Int, Int)]
gera4 = [ (x,x+1) | x <- [1..16], odd x ]

--e) gera5: a partir da lista de duplas geradas no item d,
--gerar a lista onde cada elemento corresponde à soma dos
--elementos da dupla.
gera5::[Int]
gera5 = [ x+y | (x,y) <- gera4]

--5) a) Escreva uma função (usando compreensão de listas) que calcula a quantidade de
--números que são negativos e múltiplos de 2 (ao mesmo tempo) de uma lista de inteiros:
contaNegM2::[Int] -> Int
contaNegM2 lista = length([x | x <- lista, x < 0, (mod x 2 == 0) ])

--b) Escreva uma função (usando compreensão de listas) que extrai números que são
--negativos e múltiplos de 2 (ao mesmo tempo) de uma lista de inteiros e os retorna em
--uma nova lista:
listaNegM2::[Int] -> [Int]
listaNegM2 lista = [x | x <- lista, x < 0, (mod x 2 == 0) ]

--6) Seja a função abaixo que recebe uma lista de pontos no plano cartesiano e calcula a
--distância de cada ponto à origem:distancias :: [(Float,Float)] -> [Float]
--distancias [] = []
--distancias ((x,y):xys) = (sqrt (x^2 + y^2)) : (distancias xys)
--Escreva uma outra versão da função distancias utilizando a construção de listas por
--compreensão
distancias::[(Float, Float)] -> [Float]
distancias lista = [sqrt(x^2 + y^2) | (x,y)<-lista]

--7) Escreva a função primos a seguir que recebe dois valores inteiros x,y e retorna todos
--os números primos que se encontram entre x e y. Obs: construir uma segunda função
--fatores que retorna todos os divisores de um número inteiro e utilizá-la na elaboração
--da função primos.
-- > primos 10 50
-- [11,13,17,19,23,29,31,37,41,43,47]
fatores::Int->[Int]
fatores n = [x | x <- [2..n-1], mod n x == 0]

primos::Int->Int->[Int]
primos x y = [ k | k <- [x..y], length(fatores k) == 0]

--8) Construa a função mmc a seguir que calcula o valor do mínimo múltiplo comum de três
--números inteiros
mdc :: Int-> Int -> Int
mdc x y | mod x y == 0 = y
  | mod y x == 0 = x
  | x > y = mdc y (mod x y)
  | x < y = mdc x (mod y x)
mmc2 :: Int -> Int -> Int
mmc2 x y 
  | x == 0 || y == 0 = 0
  | x == y = x
  | otherwise = div (x * y) (mdc x y) 

mmc :: Int -> Int -> Int ->Int
mmc x y c = mmc2 x (mmc2 y c)


--9) Escreva uma função que calcula a série a seguir, dados um número real x e o número
--de termos a serem calculados n. Obs: se preciso, use a função fromIntegral para
--converter n de Inteiro para Float.
funcao_serie::Float->Integer->Float
funcao_serie x n 
  | n == 0 = 0.0
  | mod n 2 == 0 = ((x / (fromInteger n)) + funcao_serie x (n-1))
  | otherwise = (((fromInteger n) / x) + funcao_serie x (n-1))

--10) Escreva a função fizzbuzz a seguir que recebe um inteiro n e retorna uma lista de
--strings. Para cada inteiro i entre 1 e n, a lista será composta da seguinte forma.
-- • Se i é divisível por 3, escreva “Fizz”.
-- • Se i é divisível por 5, escreva “Buzz”.
-- • Se i é divisível por ambos 3 e 5, escreva “FizzBuzz”.
-- • Caso contrário, diga “No”.
fizzbuzz::Int->[String]
fizzbuzz n 
  | n == 0 = []
  | mod n 3 ==0 && mod n 5 == 0 = (fizzbuzz (n-1))++["FizzBuzz"]
  | mod n 3 == 0 = (fizzbuzz (n-1))++["Fizz"]
  | mod n 5 == 0 = (fizzbuzz (n-1))++["Buzz"]
  | otherwise = (fizzbuzz (n-1))++["No"]


main :: IO ()
main = return ()
