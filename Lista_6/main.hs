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

lst1 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
lst2 = [20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
lst3 = [11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
lst4 = [10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
lst5 = [11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
lst6 = [1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
lst7 = [1..1000]
lst8 = [1000,999..1]
lst9 = lst1++[0]
lst10 = [0]++lst3
lst11 = lst1++[0]++lst3
lst12 = lst3++[0]++lst1

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

bubblesort2::Ord a => [a] -> ([a], Int)
bubblesort2 [] = ([],0)
bubblesort2 lista = bolhaOrd2 lista (length lista)

bolhaOrd2::Ord a => [a]->Int->([a], Int)
bolhaOrd2 lista 0 = (lista, 0)
bolhaOrd2 lista n = 
  let 
    (listaTrocada, qtdComp) = troca2 lista
    (listaOrd, qtd_total) = bolhaOrd2 listaTrocada (n-1)
  in (listaOrd, qtd_total + qtdComp)


troca2::Ord a => [a]->([a],Int)
troca2 [x] = ([x],0)
troca2 (x:y:zs)
  | x> y = (y:lista1, n1+1)
  | otherwise = (x:lista2,n2+1)
  where 
      (lista1, n1) = listaTrocada1
      listaTrocada1 =  troca2 (x:zs)
      (lista2, n2) = listaTrocada2
      listaTrocada2 = troca2 (y:zs)
	  
-- N0 de comparacoes
--   bubblesort2 lst1 => 380
--   bubblesort2 lst2 => 380
--   bubblesort2 lst3 => 380
--   bubblesort2 lst4 => 380
--   bubblesort2 lst5 => 380
--   bubblesort2 lst6 => 380
--   bubblesort2 lst7 => 999000
--   bubblesort2 lst8 => 999000
--   bubblesort2 lst9 => 420
--   bubblesort2 lst10 => 420
--   bubblesort2 lst11 => 1640
--   bubblesort2 lst12 => 1640


----------------------------------------------------------selectionsort

selectionsort2::(Ord a) => [a]->([a],Int)
selectionsort2 [] = ([],0)
selectionsort2 xs = ([x] ++ selectionsublista, qtdcmp + qtd_total)
        where 
        (x, qtdcmp) = minimo2 xs
        (selectionsublista, qtd_total) = selectionsort2(remove2 x xs)

remove2::(Ord a) => a->[a]->[a]
remove2 a [] = []
remove2 a (x:xs)
  | a==x = xs
  | otherwise = x:(remove2 a xs)

minimo2::(Ord a) => [a]->(a, Int)
minimo2 [] = undefined
minimo2 [x] = (x,0)
minimo2 (x:y:xs)
  | x <= y = (listax, q1+1)
  | otherwise = (listay, q2+1)
  where
    (listax, q1) = (minimo2 (x:xs))
    (listay, q2) = (minimo2 (y:xs))
	

-- N0 de comparacoes
--   selectionsort2 lst1 => 190
--   selectionsort2 lst2 => 190
--   selectionsort2 lst3 => 190
--   selectionsort2 lst4 => 190
--   selectionsort2 lst5 => 190
--   selectionsort2 lst6 => 190
--   selectionsort2 lst7 => 499500
--   selectionsort2 lst8 => 499500
--   selectionsort2 lst9 => 210
--   selectionsort2 lst10 => 210
--   selectionsort2 lst11 => 820
--   selectionsort2 lst12 => 820
  
----------------------------------------------------------insertionsort

insertionsort2::(Ord a) => [a] ->([a], Int)
insertionsort2 [] = ([],0)
insertionsort2 (x:xs) = 
  let 
    (listaInsert, qtdCmp) = insertionsort2 xs
    (listaOrdenada, qtd_total) = (insereOrd2 x listaInsert)
  in (listaOrdenada, qtdCmp + qtd_total)
  

insereOrd2::(Ord a) => a -> [a] -> ([a],Int)
insereOrd2 x [] = ([x],0)
insereOrd2 x (y:ys)
  | x <= y = ((x:y:ys),1)
  | otherwise = (y:lista, qtd+1)
  where
    (lista,qtd) = (insereOrd2 x ys)
	

-- N0 de comparacoes
--   insertionsort2 lst1 => 190
--   insertionsort2 lst2 => 19
--   insertionsort2 lst3 => 100
--   insertionsort2 lst4 => 118
--   insertionsort2 lst5 => 113
--   insertionsort2 lst6 => 139
--   insertionsort2 lst7 => 499500
--   insertionsort2 lst8 => 999
--   insertionsort2 lst9 => 210
--   insertionsort2 lst10 => 120
--   insertionsort2 lst11 => 540
--   insertionsort2 lst12 => 530

-----------------------------------------------------------quicksort

quicksort2::(Ord a) => [a] -> ([a], Int)
quicksort2 [] = ([], 0)
quicksort2 (s:xs) = 
  let 
    (lista1, qtd1) = quicksort2 [x|x <- xs, x<s]
    (lista2, qtd2) = quicksort2 [x|x <- xs, x>=s]
    qtd = length(xs) * 2
  in ((lista1++[s]++lista2), qtd1+qtd2+qtd)


-- N0 de comparacoes
--   quicksort2 lst1 => 380
--   quicksort2 lst2 => 380
--   quicksort2 lst3 => 200
--   quicksort2 lst4 => 200
--   quicksort2 lst5 => 160
--   quicksort2 lst6 => 168
--   quicksort2 lst7 => 999000
--   quicksort2 lst8 => 999000
--   quicksort2 lst9 => 382
--   quicksort2 lst10 => 240
--   quicksort2 lst11 => 840
--   quicksort2 lst12 => 480


-- 12) Altere cada um dos algoritmos de ordenação do exercício 10 para que façam
-- ordenação decrescente e também retornem o número de comparações. Teste
-- com as listas dadas. Exemplo:

----------------------------------------------------------bubblesort

bubblesort3::Ord a => [a] -> ([a], Int)
bubblesort3 [] = ([],0)
bubblesort3 lista = bolhaOrd3 lista (length lista)

bolhaOrd3::Ord a => [a]->Int->([a], Int)
bolhaOrd3 lista 0 = (lista, 0)
bolhaOrd3 lista n = 
  let 
    (listaTrocada, qtdComp) = troca3 lista
    (listaOrd, qtd_total) = bolhaOrd3 listaTrocada (n-1)
  in (listaOrd, qtd_total + qtdComp)


troca3::Ord a => [a]->([a],Int)
troca3 [x] = ([x],0)
troca3 (x:y:zs)
  | x > y = (x:lista3,n3+1)
  | otherwise = (y:lista1, n1+1)
  where 
      (lista1, n1) = listaTrocada1
      listaTrocada1 =  troca3 (x:zs)
      (lista3, n3) = listaTrocada3
      listaTrocada3 = troca3 (y:zs)

-- N0 de comparacoes
--   bubblesort3 lst1 => 380
--   bubblesort3 lst2 => 380
--   bubblesort3 lst3 => 380
--   bubblesort3 lst4 => 380
--   bubblesort3 lst5 => 380
--   bubblesort3 lst6 => 380
--   bubblesort3 lst7 => 999000
--   bubblesort3 lst8 => 999000
--   bubblesort3 lst9 => 420
--   bubblesort3 lst10 => 420
--   bubblesort3 lst11 => 1640
--   bubblesort3 lst12 => 1640

----------------------------------------------------------selectionsort

selectionsort3::(Ord a) => [a]->([a],Int)
selectionsort3 [] = ([],0)
selectionsort3 xs = ([x] ++ selectionsublista, qtdcmp + qtd_total)
        where 
        (x, qtdcmp) = maximo3 xs
        (selectionsublista, qtd_total) = selectionsort3(remove3 x xs)

remove3::(Ord a) => a->[a]->[a]
remove3 a [] = []
remove3 a (x:xs)
  | a==x = xs
  | otherwise = x:(remove3 a xs)

maximo3::(Ord a) => [a]->(a, Int)
maximo3 [] = undefined
maximo3 [x] = (x,0)
maximo3 (x:y:xs)
  | x <= y = (listay, q3+1)
  | otherwise = (listax, q1+1)
  where
    (listax, q1) = (maximo3 (x:xs))
    (listay, q3) = (maximo3 (y:xs))

-- N0 de comparacoes
--   selectionsort3 lst1 => 190
--   selectionsort3 lst2 => 190
--   selectionsort3 lst3 => 190
--   selectionsort3 lst4 => 190
--   selectionsort3 lst5 => 190
--   selectionsort3 lst6 => 190
--   selectionsort3 lst7 => 499500
--   selectionsort3 lst8 => 499500
--   selectionsort3 lst9 => 210
--   selectionsort3 lst10 => 210
--   selectionsort3 lst11 => 820
--   selectionsort3 lst12 => 820
   
  
----------------------------------------------------------insertionsort

insertionsort3::(Ord a) => [a] ->([a], Int)
insertionsort3 [] = ([],0)
insertionsort3 (x:xs) = 
  let 
    (listaInsert, qtdCmp) = insertionsort3 xs
    (listaOrdenada, qtd_total) = (insereOrd3 x listaInsert)
  in (listaOrdenada, qtdCmp + qtd_total)
  

insereOrd3::(Ord a) => a -> [a] -> ([a],Int)
insereOrd3 x [] = ([x],0)
insereOrd3 x (y:ys)
  | x <= y = (y:lista, qtd+1)
  | otherwise = ((x:y:ys),1)
  where
    (lista,qtd) = (insereOrd3 x ys)

-- N0 de comparacoes
--   insertionsort3 lst1 => 190
--   insertionsort3 lst2 => 19
--   insertionsort3 lst3 => 100
--   insertionsort3 lst4 => 118
--   insertionsort3 lst5 => 113
--   insertionsort3 lst6 => 139
--   insertionsort3 lst7 => 499500
--   insertionsort3 lst8 => 999
--   insertionsort3 lst9 => 210
--   insertionsort3 lst10 => 120
--   insertionsort3 lst11 => 540
--   insertionsort3 lst12 => 530

-----------------------------------------------------------quicksort

quicksort3::(Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 (s:xs) = 
  let 
    (lista1, qtd1) = quicksort3 [x|x <- xs, x<s]
    (lista2, qtd3) = quicksort3 [x|x <- xs, x>=s]
    qtd = length(xs) * 2
  in ((lista2++[s]++lista1), qtd1+qtd3+qtd)


-- N0 de comparacoes
--   quicksort3 lst1 => 380
--   quicksort3 lst2 => 380
--   quicksort3 lst3 => 200
--   quicksort3 lst4 => 200
--   quicksort3 lst5 => 160
--   quicksort3 lst6 => 168
--   quicksort3 lst7 => 999000
--   quicksort3 lst8 => 999000
--   quicksort3 lst9 => 382
--   quicksort3 lst10 => 240
--   quicksort3 lst11 => 840
--   quicksort3 lst12 => 480


main :: IO ()
main = return ()