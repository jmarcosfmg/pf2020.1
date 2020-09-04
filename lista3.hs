--1) Operador lógico OU (pré-fixo):
--a) Apresente 3 definições para o operador lógico OU, utilizando casamento de padrões.
(||)::Bool->Bool->Bool
True || _ = True
False || False = False
False || a = a


--b) Apresente 2 definições para o operador lógico OU, utilizando expressões condicionais
--(no lugar de casamento de padrões).

(||)::Bool->Bool->Bool
(||) x y
   | x == False && y == False = False
   | otherwise = True

--2) Defina uma função que recebe dois pontos no espaço e retorna a distância entre eles.
--Considere que um ponto no espaço é representado por uma dupla de números (float) que
--correspondem às coordenadas do ponto.

type Ponto=(Float, Float)
distancia::Ponto->Ponto->Float
distancia (x1, y1) (x2,y2) = sqrt((x1-x2)**2 + (y1 - y2)**2)

--4) Dado um valor inteiro, escreva a função recursiva fatorial. Obs: Fazer uma
--definição usando guardas e outra com casamento de padrões.

fat_guarda::Int->Int
fat_guarda x
  | x == 0 = 1
  | otherwise = x * fat_guarda (x-1)

fat_cas_padrao::Int->Int
fat_cas_padrao 0 = 1
fat_cas_padrao x = x * fat_cas_padrao(x-1)

--5) Dado um número inteiro n, escreva a função recursiva fibo que retorna o n-ésimo
--termo da sequência de Fibonacci a seguir, sendo os casos base F0 = 0 e F1 = 1. Utilize a
--definição recursiva vista em sala: fibo(n) = fibo(n-2) + fibo(n-1).

fibonacci::Int->Int
fibonacci x
  | x == 0 = 0
  | x == 1 = 1
  | otherwise = fibonacci(x-1) + fibonacci(x-2)

--6) Dado um número inteiro n, escreva a função recursiva n_tri, que retorna o n-ésimo
--termo da sequência de números triangulares, dada a seguir

n_triangular::Int->Int
n_triangular x
  | x == 0 = 0
  | otherwise = x + n_triangular(x-1)

--7) Faça uma segunda definição da função recursiva fibo2 que retorna o n-ésimo termo
--da sequência de Fibonacci utilizando recursividade e os conceitos a seguir (dica: defina a
--função passo(x,y)).
--a) Defina um par na sequência de Fibonacci como (n,n+1).
--Exemplos: (1,1), (3,5), (55,89), (233,377)
--b) Dois pares consecutivos na sequência podem ser considerados como um passo:
--(x,y) => (y, x+y). Exemplos: (1,1) => (1,2); (3,5) => (5,8); (55,89) => (89, 144)
--c) A partir do par inicial (1,1), podemos definir o enésimo par, como a aplicação
--consecutiva de n passos:
--(1,1) => (1,2) => (2,3) => (3,5) => (5,8) => (8,13) => (13,21) => (21,34) => (34,55) =>...
--d) O n-ésimo termo (para n>0) é o primeiro elemento do enésimo par.
--Ex: quarto par: (3,5) e quarto termo: 3 e décimo par: (55,89) e décimo termo: 55

passo::(Int, Int)->(Int, Int)
passo(x,y) = (y, y+x)
par_fib::(Int)->(Int,Int)
par_fib 1 = (1,1)
par_fib n = (n, n+1)
fibo2::Int->Int
fibo2 n
  | 

--8) Escreva a função potencia2, que calcula a potência de 2 elevada a um expoente n
--de forma recursiva:
potencia2::Int->Int
potencia2 0 = 1
potencia2 n = potencia2 (n -1) * 2 


--9) a) Escreva a função recursiva prodIntervalo: dados dois inteiros m e n, onde m<n,
--retorna o produto: m*(m+1)*...(n-1)*n.
prodIntervalo::Int->Int->Int
prodIntervalo m n 
  | m == n = n
  | otherwise = m * prodIntervalo (m+1) n

-- b) Reescreva a função fatorial usando a função prodIntervalo.

fatorial::Int->Int
fatorial n = prodIntervalo 1 n

--11) Defina de forma recursiva as funções resto_div e div_inteira, que retornam o
--resto e o quociente da divisão inteira de um inteiro m por inteiro n, realizando subtrações
--sucessivas de n a partir de m.
--Ex: m=20 e n=3: 20-3=17, 17-3=14, 14-3=11, 11-3=8, 8-3=5, 5-3=2.
--Como 2<3: resto=2 e quociente=6.

resto_div::Int->Int->Int
resto_div m n
  | m - n < 0 = m
  | otherwise = resto_div (m-n) n

div_inteira::Int->Int->Int
div_inteira m n
  | m - n < 0 = 0
  | otherwise = 1 + (div_inteira (m-n) n)

--12) Implemente a função mdc, usando a definição recursiva vista em sala:
--mdc(m,n) = m, se n = 0
--mdc(m,n) = mdc(n, k), se n > 0, sendo k = m mod n
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões.

mdc_guarda::(Int,Int)->Int
mdc_guarda(m,n)
  | n == 0 = m
  | otherwise = mdc_guarda(n, k)
  where
      k = mod m n

mdc_cas_padrao::(Int,Int)->Int
mdc_cas_padrao(m, 0) = m
mdc_cas_padrao(m, n) = mdc_cas_padrao(n, (mod m n))

--13) Implemente a função binomial usando a definição recursiva vista em sala:
---binomial (n,k) = 1, se k = 0
---binomial (n,k) = 1, se k = n
---binomial (n,k) = binomial (n-1,k) + binomial (n-1,k-1), se 0 < k < n
--Observe que binomial (n,k) não é definido se k>n.
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões.

binog :: (Int,Int) -> Int
binog (n,k)
  | k == 0 = 1
  | k == n = 1
  | otherwise = binog (n-1,k) + binog (n-1,k-1)

binomial :: (Int,Int) -> Int
binomial (n,0) = 1
binomial (n,k) = if (k == n)
      then 1
      else binomial (n-1,k) + binomial (n-1,k-1)

--14) Gere por enumeração as seguintes listas:
--a) [5,4,3,2,1]
lista1 = [5,4..1]
--b) [a,c,e]
lista2 = ['a','c' .. 'e']
--c) [1,4,7,10,13,16]
lista3 = [1,4 .. 16]
--d) [(1,1),(-2,5),(-5,9),(-8,13),(-11,17)] *obs: também é necessário usar a função zip para criar as tuplas
lista4 = zip [1, -2 .. -11] [1, 5 .. 17]

--15) Funções que utilizam listas enumeradas
--a) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a
--lista dos inteiros contidos no intervalo fechado [a,b]. Quando a for igual a b, a função
--devolve a lista unitária [a]. Quando a > b a função deverá devolver a lista vazia.

lista5::Int->Int->[Int]
lista5 a b  = [a..b]

--b) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a
--lista dos inteiros pares contidos no intervalo aberto (a,b). Quando a for igual a b ou a > b
--a função devolve a lista vazia. (*Dica: verificar se a é par ou ímpar)

lista6 a b = [a+1 .. b-1]


main :: IO ()
main = return ()
