--1) Usando a função map, escreva a função paridade a seguir que recebe uma
--lista de inteiros l e retorna uma lista contendo os valores booleanos que indicam
--a paridade dos elementos de l.

paridade::[Int]->[Bool]
paridade lista = map even lista


-- 2) Usando a função map, escreva a função prefixos a seguir que recebe uma
-- lista de strings l e retorna uma lista contendo os três primeiros caracteres de
-- cada elemento de l.

prefixos::[String]->[String]
prefixos lista = map (take 3) lista

-- 3) Usando a função map, escreva a função saudacao a seguir que recebe uma
-- lista de nomes (strings) l e retorna uma lista contendo cada elemento de l
-- concatenado com a saudação “Oi “ na frente de cada nome

saudacao::[String]->[String]
saudacao lista = map ("Oi " ++) lista

-- 4) Reescreva a definição da função filter que já faz parte da biblioteca
-- padrão do Haskell, chamando-a de filtrar. Além disso, defina a função filtrar
-- usando lista por compreensão. 

filtrar::(a -> Bool)->[a]->[a]
filtrar f lista = [x | x<-lista, f(x)]

-- 5) Usando a função filter, escreva a função pares que recebe uma lista de
-- inteiros lst e e retorna uma lista contendo os elementos pares de lst.

pares::[Int]->[Int]
pares list = filtrar even list

-- 6) Usando a função filter, escreva a função solucoes a seguir que recebe uma
-- lista de inteiros l e retorna uma lista contendo os valores que satisfazem a equação
-- (5*x + 6) < (x*x). Use uma expressão lambda (função anônima) para
-- representar a função que realiza o teste do filtro.

solucoes::[Int]->[Int]
solucoes l = filter (\x -> (5*x +6) < (x*x)) l

-- 7) Usando a função foldr1, escreva a função maior a seguir que recebe uma lista e
-- retorna seu maior elemento.

maior::Ord a => [a] -> a
maior lista = foldr1 max lista

-- 8) Usando a função foldr, escreva a função menor_min10 a seguir que recebe
-- uma lista e retorna o menor elemento da lista, desde que este não acima de 10, Se o
-- menor elemento for um valor acima de 10, retorna 10. 

menor_min10::(Ord a, Num a) => [a] -> a
menor_min10 lista = foldr1 min ([10]++[x | x<-lista, x<10])

-- 9) Usando a função foldr, escreva a função junta_silabasplural a seguir
-- que recebe uma lista de sílabas (strings) e retorna uma palavra (string) formada pela
-- concatenação das sílabas e incluindo um “s” no final . 

junta_silabas_plural::[String]->String
junta_silabas_plural lista = foldr1 (++) (lista++["s"])

-- 10) Implemente as funções de ordenação bubblesort, selectionsort,
-- insertionsort e quicksort, conforme as definições apresentadas no
-- material de aula (ordenação crescente). Posteriormente,teste cada função
-- com os seguintes exemplos de listas e observe a diferença de desempenho
-- (tempo de processamento, dependendo da lista que é fornecida como
-- entrada.

----------------------------------------------------------bubblesort
bubblesort::Ord a => [a] -> [a]
bubblesort [] = []
bubblesort lista = bolhaOrd lista (length lista)

bolhaOrd::Ord a => [a]->Int->[a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca::Ord a => [a]->[a]
troca [x] = [x]
troca (x:y:zs)
  | x> y = y : troca (x:zs)
  | otherwise = x : troca (y:zs)
 

----------------------------------------------------------selectionsort
selectionsort::(Ord a) => [a]->[a]
selectionsort [] = []
selectionsort xs =[x] ++ selectionsort (remove x xs)
        where x = minimo xs

remove::(Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
  | a==x = xs
  | otherwise = x:(remove a xs)

minimo::(Ord a) => [a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs
  
----------------------------------------------------------insertionsort
insertionsort::(Ord a) => [a] ->[a]
insertionsort [] = []
insertionsort (x:xs) = insereOrd x (insertionsort xs)

insereOrd::(Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
  | x <= y = (x:y:ys)
  | otherwise = y: (insereOrd x ys)

-----------------------------------------------------------quicksort
quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x|x <- xs, x<s] ++ [s] ++ quicksort [x|x <- xs, x >= s]

--11) Altere cada um dos algoritmos de ordenação do exercício 10 para também
--contabilizar e retornar o número de comparações (do tipo x <= y) feitas pelo
--algoritmo em sua execução. Teste com as listas dadas. Exemplo:


----------------------------------------------------------bubblesort
bubblesort::Ord a => [a] -> [a]
bubblesort [] = []
bubblesort lista = bolhaOrd lista (length lista)

bolhaOrd::Ord a => [a]->Int->[a]
bolhaOrd lista 0 = lista
bolhaOrd lista n = bolhaOrd (troca lista) (n-1)

troca::Ord a => [a]->[a]
troca [x] = [x]
troca (x:y:zs)
  | x> y = y : troca (x:zs)
  | otherwise = x : troca (y:zs)
 

----------------------------------------------------------selectionsort
selectionsort::(Ord a) => [a]->[a]
selectionsort [] = []
selectionsort xs =[x] ++ selectionsort (remove x xs)
        where x = minimo xs

remove::(Ord a) => a->[a]->[a]
remove a [] = []
remove a (x:xs)
  | a==x = xs
  | otherwise = x:(remove a xs)

minimo::(Ord a) => [a]->a
minimo [] = undefined
minimo [x] = x
minimo (x:xs)
  | x <= (minimo xs) = x
  | otherwise = minimo xs
  
----------------------------------------------------------insertionsort
insertionsort::(Ord a) => [a] ->[a]
insertionsort [] = []
insertionsort (x:xs) = insereOrd x (insertionsort xs)

insereOrd::(Ord a) => a -> [a] -> [a]
insereOrd x [] = [x]
insereOrd x (y:ys)
  | x <= y = (x:y:ys)
  | otherwise = y: (insereOrd x ys)

-----------------------------------------------------------quicksort
quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (s:xs) = quicksort [x|x <- xs, x<s] ++ [s] ++ quicksort [x|x <- xs, x >= s]


main :: IO ()
main = return ()