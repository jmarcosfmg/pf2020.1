--Aluno João Marcos de Freitas Moreira Gomes | 11611BCC043

l1=[1..1000]
l2=[1000,999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6] 

--Exercício 1) Em relação à implementações dos algoritmos seleção e inserção vistas em aula:
-- a) Refaça a implementação do algoritmo Seleção usando funções genéricas (foldr ou foldr1) .

selecao::Ord a => [a] ->[a]
selecao [] = []
selecao [x] = [x]
selecao lista = 
  let 
    y = (foldr1 min lista)
    sublista = (retira lista y)
    retira [] x = [] 
    retira (l:ls) x 
      | l == x = ls
      | otherwise = l:(retira ls x)
  in y:(selecao sublista)
   

--b) Refaça a implementação do algoritmo Inserção usando funções genéricas (foldr ou foldr1).

insercao::(Ord a) => [a]->[a]
insercao [] = []
insercao lista = foldr insereOrd [] lista

insereOrd::(Ord a) => a->[a]->[a]
insereOrd x [] = [x]
insereOrd x (y:ys)
  |x <= y    = (x:y:ys)
  |otherwise = y: (insereOrd x ys)

-- c) Refaça a implementação do algoritmo quicksort usando funções genéricas (filter): modifique a função principal do algoritmo (quicksort) para que seja utilizada a função de alta ordem (genérica) filter para a obtenção dos elementos maiores e menores do que o pivô a cada iteração.

quicksort::(Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let 
    listamenor = filter (< x) xs
    listamaior = filter (>= x) xs
  in (quicksort listamenor) ++ [x] ++ (quicksort listamaior)


-- Exercício 2) Em relação ao algoritmo de ordenação Bolha visto em sala e laboratório:
-- Variação 1: parada do algoritmo é antecipada quando uma iteração de trocas é finalizada sem que nenhuma troca efetiva seja realizada na iteração completa.

bolha1::(Ord a) => [a]->[a]
bolha1 [] = []
bolha1 lista 
  | numTrocas == 0 = listatrocada
  | otherwise = bolha1 listatrocada
  where (listatrocada, numTrocas) = trocaOrd1 lista


trocaOrd1::(Ord a) => [a] ->([a], Int)
trocaOrd1 [] = ([], 0)
trocaOrd1 [x] = ([x], 0)
trocaOrd1 (x:y:xs)
  | y < x = (y:listatrocada1, trocas1+1)
  | otherwise = (x:listatrocada2, trocas2)
  where 
    (listatrocada1, trocas1) = trocaOrd1 (x:xs)
    (listatrocada2, trocas2) = trocaOrd1 (y:xs)


-- Variação 2: a cada iteração de trocas, a avaliação é realizada desconsiderando-se o último elemento cuja posição foi fixada. Ou seja, a lista a ser ordenada é diminuída.

bolha2::(Ord a) => [a]->[a]
bolha2 [] = []
bolha2 lista = bolhaOrd2 lista (length lista)

bolhaOrd2::(Ord a) => [a]->Int->[a]
bolhaOrd2 lista 0 = lista 
bolhaOrd2 lista n = bolhaOrd2 (troca2 lista (n-1)) (n-1)

troca2::(Ord a) => [a]->Int->[a] --faz "n-1" comparações
troca2 lista 0 = lista
troca2 (x:y:zs) n
 |x > y     = y: troca2 (x:zs) (n-1)
 |otherwise = x: troca2 (y:zs) (n-1)


-- Variação 3: compões as duas versões anteriores: faz parada antecipada e diminui o tamanho da lista a ser ordenada a cada iteração.

bolha3::(Ord a) => [a]->[a]
bolha3 [] = []
bolha3 lista 
  | numTrocas == 0 = listatrocada
  | otherwise = bolha3 listatrocada
  where (listatrocada, numTrocas) = trocaOrd3(lista,length(lista))

trocaOrd3::(Ord a) => ([a], Int) ->([a], Int)
trocaOrd3 ([],_) = ([], 0)
trocaOrd3 ([x],_) = ([x], 0)
trocaOrd3 (lista, 0) = (lista, 0)
trocaOrd3 ((x:y:xs), n)
  | y < x = (y:listatrocada1, trocas1+1)
  | otherwise = (x:listatrocada2, trocas2)
  where 
    (listatrocada1, trocas1) = trocaOrd3 ((x:xs), n-1)
    (listatrocada2, trocas2) = trocaOrd3 ((y:xs), n-1)

-- Repetir as variações anteriores, incluindo um contador de comparações elementares realizadas durante a ordenação.


bolha1_cont::(Ord a) => [a]->([a], Int)
bolha1_cont [] = ([], 0)
bolha1_cont lista 
  | numTrocas == 0 = (listatrocada, cont)
  | otherwise = (lista, qtd+cont)
  where 
    (listatrocada, numTrocas, cont) = trocaOrd1_cont lista
    (lista, qtd) = (bolha1_cont listatrocada)


trocaOrd1_cont::(Ord a) => [a] ->([a], Int, Int)
trocaOrd1_cont [] = ([], 0, 0)
trocaOrd1_cont [x] = ([x], 0, 0)
trocaOrd1_cont (x:y:xs)
  | y < x = (y:listatrocada1, trocas1+1, qtd1+1)
  | otherwise = (x:listatrocada2, trocas2, qtd2+1)
  where 
    (listatrocada1, trocas1, qtd1) = trocaOrd1_cont (x:xs)
    (listatrocada2, trocas2, qtd2) = trocaOrd1_cont (y:xs)


bolha2_cont::(Ord a) => [a]->([a], Int)
bolha2_cont [] = ([], 0)
bolha2_cont lista = bolhaOrd2_cont lista (length lista)

bolhaOrd2_cont::(Ord a) => [a]->Int->([a],Int)
bolhaOrd2_cont lista 0 = (lista , 0)
bolhaOrd2_cont lista n = 
  let
    (lista1, qtd1) = (troca2_cont lista (n-1))
    (lista2, qtd2) = bolhaOrd2_cont lista1 (n-1)
  in 
    (lista2, qtd1+qtd2)

troca2_cont::(Ord a) => [a]->Int->([a], Int) --faz "n-1" comparações
troca2_cont lista 0 = (lista, 0)
troca2_cont (x:y:zs) n
 |x > y     = (y:lista1, qtd1+1) 
 |otherwise = (x:lista2, qtd2+1)
  where
  (lista1, qtd1) = (troca2_cont (x:zs) (n-1))
  (lista2, qtd2) = (troca2_cont (y:zs) (n-1))


bolha3_cont::(Ord a) => [a]->([a], Int)
bolha3_cont [] = ([], 0)
bolha3_cont lista 
  | numTrocas == 0 = (listatrocada, cont)
  | otherwise = (lista, cont+qtd)
  where 
  (listatrocada, numTrocas, cont) = trocaOrd3_cont(lista,length(lista))
  (lista, qtd) = bolha3_cont listatrocada

trocaOrd3_cont::(Ord a) => ([a], Int) ->([a], Int, Int)
trocaOrd3_cont ([],_) = ([], 0, 0)
trocaOrd3_cont ([x],_) = ([x], 0, 0)
trocaOrd3_cont (lista, 0) = (lista, 0, 0)
trocaOrd3_cont ((x:y:xs), n)
  | y < x = (y:listatrocada1, trocas1+1, qtd1+1)
  | otherwise = (x:listatrocada2, trocas2, qtd2+1)
  where 
    (listatrocada1, trocas1, qtd1) = trocaOrd3_cont ((x:xs), n-1)
    (listatrocada2, trocas2, qtd2) = trocaOrd3_cont ((y:xs), n-1)


-- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos das variações e do algoritmo original para avaliar: 
--1) o número de comparações em cada execução;
--2) o tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de processamento)

{-Conclusão: A melhor variação é a teceira, que combina a primeira com a segunda
Uma vez que essa variação faz a ordenação parar quando a lista já está ordenada
e também fixa o maior elemento na sua posição sem que haja comparações desnecessárias
-}

--Exercício 3) Em relação ao algoritmo de ordenação Selecao visto em sala de aula e laboratório:
-- Variação 1: apenas modifique a função principal do algoritmo (seleção) para que não seja utilizada uma concatenação de listas a cada iteração, no passo [x] ++ selecao (remove x xs). Ao invés disso, utilize o operador de construção de listas “:”, como em a:b.

selectionsort1::(Ord a) => [a]->[a]
selectionsort1 [] = []
selectionsort1 xs =x:(selectionsort1 (remove1 x xs))
        where x = minimo1 xs

remove1::(Ord a) => a->[a]->[a]
remove1 a [] = []
remove1 a (x:xs)
  | a==x = xs
  | otherwise = x:(remove1 a xs)

minimo1::(Ord a) => [a]->a
minimo1 [] = undefined
minimo1 [x] = x
minimo1 (x:xs)
  | x <= (minimo1 xs) = x
  | otherwise = minimo1 xs

-- Variação2: a partir da Variação 1, refazer o código para que a busca pelo menor elemento (função mínimo) e a eliminação desse menor elemento da lista a ser ordenada (função remove) ocorra numa mesma função (remove_menor), sem a necessidade de se percorrer a lista duas vezes a cada iteração (uma para remover e outra para remover o menor elemento).

selectionsort2::(Ord a) => [a]->[a]
selectionsort2 [] = []
selectionsort2 xs = x:(selectionsort2 lista)
        where (lista, x) = removeminimo xs

removeminimo::(Ord a) =>[a]->([a], a)
removeminimo [] = undefined
removeminimo [x] = ([], x)
removeminimo (x:y:xs) 
  | x <= y = (y:lista1, min1)
  | otherwise = (x:lista2, min2)
  where 
    (lista1, min1) = (removeminimo (x:xs))
    (lista2, min2) = (removeminimo (y:xs))


-- Repetir a Variação 2, incluindo um contador de comparações elementares realizadas durante a ordenação. 

selectionsort3::(Ord a) => [a]->([a], Int)
selectionsort3 [] = ([], 0)
selectionsort3 xs = (x:lista, qtd+cont)
        where 
          (lista, x, cont) = removeminimo2 xs
          (lista2, qtd) = selectionsort3 lista

removeminimo2::(Ord a) =>[a]->([a], a, Int)
removeminimo2 [] = undefined
removeminimo2 [x] = ([], x, 0)
removeminimo2 (x:y:xs) 
  | x <= y = (y:lista1, min1, qtd1+1)
  | otherwise = (x:lista2, min2, qtd2+1)
  where 
    (lista1, min1, qtd1) = (removeminimo2 (x:xs))
    (lista2, min2, qtd2) = (removeminimo2 (y:xs))

-- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos da Variação 2 e do algoritmo original para avaliar: 1) o número de comparações em cada execução;

selectionsortoriginal::(Ord a) => [a]->([a],Int)
selectionsortoriginal [] = ([],0)
selectionsortoriginal xs = ([x] ++ selectionsublista, qtdcmp + qtd_total+ qtdrem)
        where 
        (x, qtdcmp) = minimo2 xs
        (lista, qtdrem) = remove2 x xs
        (selectionsublista, qtd_total) = selectionsortoriginal(lista)

remove2::(Ord a) => a->[a]->([a], Int)
remove2 a [] = ([], 0)
remove2 a (x:xs)
  | a==x = (xs, 1)
  | otherwise = (x:lista, qtd+1)
  where (lista, qtd) = (remove2 a xs)

minimo2::(Ord a) => [a]->(a, Int)
minimo2 [] = undefined
minimo2 [x] = (x,0)
minimo2 (x:y:xs)
  | x <= y = (listax, q1+1)
  | otherwise = (listay, q2+1)
  where
    (listax, q1) = (minimo2 (x:xs))
    (listay, q2) = (minimo2 (y:xs))


-- n comparacoes
--lista | original | variacao2
--  l1  |  500500  |  499500
--  l2  |  1000000 |  499500
--  l3  |  502501  |  500500
--  l4  |  1001001 |  500500
--  l5  |  3003001 |  2001000
--  l6  |  3003001 |  2001000
--  l7  |  3502501 |  2001000
--  x1  |  210     |  190
--  x2  |  400     |  190
--  x3  |  310     |  190
--  x4  |  300     |  190
--  x5  |  305     |  190
--  x6  |  276     |  190
--  x7  |  304     |  190


-- 2) o tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de processamento). 
-- Eleja a melhor variação do algoritmo Seleção, justificando sua escolha. 

-- {Conclusão: O melhor algorítmo de seleção foi o da segunda versão, pois apresentou menores quantidades de comparações e uma velocidade mais rápida }

-- Exercício 4) Em relação ao algoritmo de ordenação quicksort visto em sala de aula e laboratório:
-- Variação 1: modifique o algoritmo original para que ao invés dos elementos maiores e menores serem encontrados com buscas independentes, que seja elaborada e utilizada a função divide que percorre a lista uma única vez, retornando os elementos menores em uma lista e os elementos menores em outra.
--  EX: > divide 'j' "pindamonhangaba" Resposta: ("idahagaba","pnmon

divide::(Ord a) => a -> [a] -> ([a], [a])
divide _ [] = ([],[])
divide x (y:ys)
  | x < y = (lista1, y:lista2)
  | otherwise = (y:lista1, lista2)
  where 
    (lista1, lista2) = (divide x ys)

quicksort1::(Ord a) => [a] -> [a]
quicksort1 [] = []
quicksort1 (s:xs) = 
  let 
    (lista1, lista2) = (divide s xs)
  in ((quicksort1 lista1)++[s]++(quicksort1 lista2))

-- Variação 2: modifique a variação 1 para que o elemento pivô seja obtido a partir da análise dos 3 primeiros elementos da lista, sendo que o pivô será o elemento mediano entre eles. Exemplo: na lista [3, 9, 4, 7, 8, 1, 2], os elementos 3, 9 e 4 seriam analisados e o pivô escolhido seria 4. Caso a lista a ser analisada tenha menos que 3 elementos, o pivô é sempre o primeiro.

pegapivo::(Ord a)=>[a] -> (a, [a])
pegapivo [] = undefined
pegapivo lista
  | length(lista) < 3 = (y, ys)
  | otherwise = (bubble lista 1)
  where 
    (y:ys) = lista
    bubble (x:y:xs) n
      | n == 0 && x <= y = (x,(y:xs))
      | n == 0 = (y, (x:xs))
      | x > y = (pivo1, y:lista1)
      | otherwise = (pivo2, x:lista2)
      where 
        (pivo1, lista1) = (bubble (x:xs) (n-1))
        (pivo2, lista2) = (bubble (y:xs) (n-1))

quicksort2::(Ord a)=>[a] -> [a]
quicksort2 [] = []
quicksort2 lista = 
  let 
    (pivo, listasempivo) = pegapivo lista
    (lista1, lista2) = (divide pivo listasempivo)
  in ((quicksort2 lista1)++[pivo]++(quicksort2 lista2))

-- Repetir as variações 1 e 2, incluindo um contador de comparações elementares realizadas durante a ordenação.


divide1::(Ord a) => a -> [a] -> ([a], [a], Int)
divide1 _ [] = ([],[], 0)
divide1 x (y:ys)
  | x < y = (lista1, y:lista2, qtd+1)
  | otherwise = (y:lista1, lista2, qtd+1)
  where 
    (lista1, lista2, qtd) = (divide1 x ys)

quicksort3::(Ord a) => [a] -> ([a], Int)
quicksort3 [] = ([], 0)
quicksort3 (s:xs) = 
  let 
    (lista1, lista2, qtd1) = (divide1 s xs)
    (qlista1, qtdlista1) = (quicksort3 lista1)
    (qlista2, qtdlista2) = (quicksort3 lista2)
  in ((qlista1++[s]++qlista2), qtd1+qtdlista1+qtdlista2)


pegapivo1::(Ord a)=>[a] -> (a, [a], Int)
pegapivo1 [] = undefined
pegapivo1 lista
  | length(lista) < 3 = (y, ys, 0)
  | otherwise = (bubble lista 1)
  where 
    (y:ys) = lista
    bubble (x:y:xs) n
      | n == 0 && x <= y = (x,(y:xs), 1)
      | n == 0 = (y, (x:xs), 1)
      | x > y = (pivo1, y:lista1, qtd1+1)
      | otherwise = (pivo2, x:lista2, qtd2+1)
      where 
        (pivo1, lista1, qtd1) = (bubble (x:xs) (n-1))
        (pivo2, lista2, qtd2) = (bubble (y:xs) (n-1))

quicksort4::(Ord a)=>[a] -> ([a], Int)
quicksort4 [] = ([], 0)
quicksort4 lista = 
  let 
    (pivo, listasempivo, qtd1) = pegapivo1 lista
    (lista1, lista2, qtd2) = (divide1 pivo listasempivo)
    (listaesq, qtdesq) = (quicksort4 lista1)
    (listadir, qtddir) = (quicksort4 lista2)
  in ((listaesq++[pivo]++listadir), qtd1+qtd2+qtdesq+qtddir)

-- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos da Variação 1, 2 e do algoritmo original para avaliar: 1) o número de comparações em cada execução; 2) o tempo de execução (apenas verifiquem se existe uma mudança aparente de tempo de processamento).

quicksortoriginal::(Ord a) => [a] -> ([a], Int)
quicksortoriginal [] = ([], 0)
quicksortoriginal (s:xs) = 
  let 
    (lista1, qtd1) = quicksortoriginal [x|x <- xs, x<s]
    (lista2, qtd3) = quicksortoriginal [x|x <- xs, x>=s]
    qtd = length(xs) * 2
  in ((lista1++[s]++lista2), qtd1+qtd3+qtd)

-- n comparacoes
--lista | original | variacao1  |  variacao2
--  l1  |  999000  |  499500    |   250998
--  l2  |  999000  |  499500    |   250998
--  l3  |  999002  |  499501    |   251000
--  l4  |  1001000 |  500500    |   251500
--  l5  |  2002000 |  1000002   |   503503
--  l6  |  2002000 |  1002000   |   338662
--  l7  |  2002000 |  1002000   |   338665
--  x1  |  380     |  190       |   118
--  x2  |  380     |  190       |   88
--  x3  |  200     |  100       |   83
--  x4  |  200     |  100       |   76
--  x5  |  160     |  80        |   78
--  x6  |  168     |  84        |   87
--  x7  |  162     |  81        |   82

-- Eleja a melhor variação do algoritmo QuickSort, justificando sua escolha. 

-- {Conclusão: O melhor algorítmo de ordenação quicksort foi o da segunda versão, pois apresentou menores quantidades de comparações eem quase todos os casos e uma velocidade mais rápida na ordenação}

-- Exercício 5)
-- Pesquise e implemente sua própria versão em Haskell dos algoritmos mergesort e bucketsort

mergeSort::(Ord a) => [a]->[a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort lista = merge (mergeSort primeira) (mergeSort segunda) 
    where primeira = take ((length lista) `div` 2) lista 
          segunda = drop ((length lista) `div` 2) lista

merge::(Ord a) => [a]->[a]->[a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) 
  | x < y     = x:(merge xs (y:ys)) 
  | otherwise = y:(merge (x:xs) ys)

-- Repetir os algoritmos anteriores, incluindo um contador de comparações elementares realizadas durante a ordenação. 

mergeSort2::(Ord a) => [a]->([a],Int)
mergeSort2 [] = ([],0)
mergeSort2 [x] = ([x],0)
mergeSort2 lista = (l3,n1+n2+n3)
    where 
      primeira = take ((length lista) `div` 2) lista
      segunda= drop ((length lista) `div` 2) lista
      (l1,n1) = mergeSort2 primeira
      (l2,n2) = mergeSort2 segunda
      (l3,n3) = merge2 l1 l2

merge2::(Ord a) => [a]->[a]->([a],Int)
merge2 x [] = (x,0)
merge2 [] x = (x,0)
merge2 (x:xs) (y:ys)
  | x < y     = (x:l1,n1+1)
  | otherwise = (y:l2,n2+1)
   where
    (l1,n1) = (merge2 xs (y:ys))
    (l2,n2) = (merge2 (x:xs) ys)


-- Realizar execuções comparativas nas listas dadas como exemplo entre os algoritmos
-- selecionados como as melhores versões do Bolha, Inserção, Seleção e Quicksort, além do
-- algoritmo Mergesort, para avaliar: 1) o número de comparações em cada execução; 2) o tempo
-- de execução (apenas verifiquem se existe uma mudança aparente de tempo de processamento). 


{-Conclusão: Levando-se em conta o tempo de execução e a quantidade de comparações
realizadas durante a ordenação, o Merge Sort se mostrou melhor, uma vez que ele
se mantém relativamente constante(apresenta valores próximos) no tempo e nas comparações,
independetemente, se a lista está totalmente organizada ou não
-}

------------------------------------------------PARTE B – Tipos Algébricos e Ávores
--Exercício 6) Dada a definição de tipos abaixo, similar à vista em aula
-- a) Expanda as definições acima para que além das operações soma e subtração, sejam incluídas expressões usando as operações multiplicação e potenciação. 

data Exp a = Val a
             | Add (Exp a) (Exp a)
             | Sub (Exp a) (Exp a)
             | Mul (Exp a) (Exp a)
             | Div (Exp a) (Exp a)
             | Pow (Exp a) (Exp a)
  deriving(Show,Eq)         

avalia::(Num a,Floating a)=> Exp a -> a
avalia (Val x) = x
avalia (Add exp1 exp2) = (avalia exp1) + (avalia exp2)
avalia (Sub exp1 exp2) = (avalia exp1) - (avalia exp2) 
avalia (Mul exp1 exp2) = (avalia exp1) * (avalia exp2)
avalia (Pow exp1 exp2) = (avalia exp1) ** (avalia exp2)

-- b) Avalie as expressões abaixo, primeiro declarando-as de acordo com a sintaxe do
-- tipo algébrico e depois executando a função avalia sobre essas declarações:
-- (3+12)*(15-5)^(1*3)
expre1::Num a=> Exp a
expre1 = (Pow(Mul(Add(Val 3)(Val 12)) (Sub (Val 15)(Val 5)))
             (Pow (Val 1)(Val 3)))


-- -((6+8-5+1)*(2+6^2))
expre2::Num a=> Exp a
expre2 = (Sub(Val 0)(Mul(Add(Sub(Add(Val 6)(Val 8))(Val 5))(Val 1)) 
                 (Add(Val 2) (Pow(Val 6)(Val 2)))))


-- Exercício 7) Defina um tipo algébrico Hora para modelar os horários do dia usando a convenção AM (antes do meio-dia) e PM (após o meio-dia), que deve armazenar as horas e os minutos. Os valores do tipo Hora são escritos na forma (AM x y) ou (PM x y), sendo x e y valores do tipo Int.

data Hora = AM Int Int |
            PM Int Int
            deriving(Show, Eq, Ord)


-- a) Implemente a função horasDecorridas, que recebe um horário do dia, definido pelo tipo algébrico Hora, e essa função deve retornar a quantidade de horas decorridas no dia até o horário passado como argumento.

horasDecorridas::Hora->Int
horasDecorridas (AM x _) = x
horasDecorridas (PM x _) = 12+x

-- De forma similar, implemente as funções minutosDecorridos e segundosDecorridos que
-- devem retornar a quantidade de minutos/segundos decorridas no dia até o horário.

minutosDecorridos::Hora->Int
minutosDecorridos (AM x y) = (horasDecorridas (AM x y))*60 + y
minutosDecorridos (PM x y) = (horasDecorridas (PM x y))*60 + y

segundosDecorridos::Hora->Int
segundosDecorridos (AM x y) = (minutosDecorridos (AM x y))*60
segundosDecorridos (PM x y) = (minutosDecorridos (PM x y))*60



-- b) Modifique as funções do item a, para que sejam rejeitados valores de hora que estejam fora do intervalo de 0 a 11 e valores de minuto fora do intervalo de 0 a 59.

horasDecorridas1::Hora->Int
horasDecorridas1 (AM x y) 
  | x < 0 || x > 12 = error "Hora fora do padrão"
  | y < 0 || y > 59 = error "Minuto fora do padrão"
  | otherwise = x
horasDecorridas1 (PM x y)
  | x < 0 || x > 12 = error "Hora fora do padrão"
  | y < 0 || y > 59 = error "Minuto fora do padrão"
  | otherwise = x+12

minutosDecorridos1::Hora->Int
minutosDecorridos1 (AM x y) = (horasDecorridas1 (AM x y))*60 + y
minutosDecorridos1 (PM x y) = (horasDecorridas1 (PM x y))*60 + y

segundosDecorridos1::Hora->Int
segundosDecorridos1 (AM x y) = (minutosDecorridos1 (AM x y))*60
segundosDecorridos1 (PM x y) = (minutosDecorridos1 (PM x y))*60

-- c) Faça as alterações necessárias no código para que os testes abaixo possam ser realizados com sucesso no console do GHC 
 -- pronto - foi acrescentado deriving(Show, Eq, Ord)

--Exercício 8) Defina um Tipo Algébrico para registrar mensagens de texto recebidas, podendo ser provenientes de LinkedIn, WhatsApp ou Facebook. O objetivo é realizar a união destas mensagens de forma a agilizar a interação das pessoas com os seus contatos profissionais. Cada mensagem deve ter: identificador do remetente, curta mensagem (até 100 caracteres), data, hora e proveniência, como nos exemplos: 


type Msg = String
data App = WhatsApp | Facebook | LinkedIn
            deriving(Show, Eq)
data Data = Data Int Int Int deriving(Show, Eq, Ord)

data Hora = AM Int Int |
            PM Int Int
            deriving(Show, Eq, Ord)

msgCurta mensagem = take 100 mensagem

data Identificador = Fone String | Nome String
            deriving(Show, Eq)
data Registro = Contato {
              identificador::Identificador,
              mensagem:: Msg,
              dia::Data,
              hora:: Hora,
              app:: App}deriving(Show, Eq)

--a) Crie uma pequena base de mensagens com pelo menos 30 ocorrências em dois dias consecutivos de Set/2020 para que seja possível testar as funções definidas abaixo, repetindo contatos em diferentes Apps (como o exemplo do Augusto Costa acima) Analise os componentes de uma mensagem e defina outros tipos algébricos se necessário. 

msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9,msg10,msg11,msg12,msg13,msg14,msg15,msg16,msg17,msg18,msg19,msg20,msg21,msg22,msg23,msg24,msg25,msg26,msg27,msg28,msg29,msg30::Registro
msg1 = Contato {identificador=(Nome "Paula Troiano"), mensagem= "gam em dezembro. Deve ser por volta da virada do ano. Se isso ocorrer mesmo, tende a suavizar um pou" ,dia = (Data 01 09 2020), hora= (AM 10 00),app= LinkedIn}
msg2 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "O ritmo de retomada economica tem surpreendido. Eh sustentavel? Existe um padrao muito claro na retom",dia = (Data 01 09 2020), hora= (AM 10 30),app= WhatsApp}
msg3 = Contato {identificador=(Nome "Vinicius Costa"), mensagem= "ada economica pos pandemia. Os papers academicos estão começando a mostrar que o grande motor da que" ,dia = (Data 01 09 2020), hora= (AM 10 31),app= Facebook}
msg4 = Contato {identificador=(Nome "Denis Costa"), mensagem= "da da atividade nao foram as restrições impostas pelos paises, mas sim o medo. Com a redução do nive" ,dia = (Data 01 09 2020), hora= (AM 10 35),app= LinkedIn}
msg5 = Contato {identificador=(Fone "99445-5700"), mensagem= "l da contaminaçao e das mortes, as pessoas se sentem mais seguras para saírem a s ruas. Então, ocorre" ,dia = (Data 01 09 2020), hora= (AM 11 30),app= WhatsApp}
msg6 = Contato {identificador=(Nome "Paula Troiano"), mensagem= "o mesmo descasamento do início da pandemia, em que houve queda rapida da demanda, enquanto a produc" ,dia = (Data 01 09 2020), hora= (AM 11 30),app= WhatsApp}
msg7 = Contato {identificador=(Nome "Denis Costa"), mensagem= "ao foi 'desligada' mais lentamente, mas de maneira inversa: a demanda volta, mas a produção ainda na" ,dia = (Data 01 09 2020), hora= (PM 0 30),app= Facebook}
msg8 = Contato {identificador=(Nome "Vinicius Costa"), mensagem= "o voltou. E o que tinha de estoque no início, que inicialmente parecia muito, foi sendo gasto nos va" ,dia = (Data 01 09 2020), hora= (PM 1 30),app= LinkedIn}
msg9 = Contato {identificador=(Fone "99445-5700"), mensagem= "rios meses em que tudo ficou parado na economia.  E qual é a consequencia? O comercio esta acima do " ,dia = (Data 01 09 2020), hora= (PM 2 50),app= WhatsApp}
msg10 = Contato {identificador=(Nome "Catalina Lina"), mensagem= "patamar pré-crise na maioria dos países. Isso tudo porque há uma coincidência temporal da demanda, p" ,dia = (Data 01 09 2020), hora= (PM 2 55),app= LinkedIn}
msg11 = Contato {identificador=(Nome "Denis Costa"), mensagem= "ois a população sai as ruas ao mesmo tempo, ha consumo reprimido e algum dinheiro no bolso, por caus" ,dia = (Data 01 09 2020), hora= (PM 6 30),app= Facebook}
msg12 = Contato {identificador=(Nome "Denis Costa"), mensagem= "a da ajuda governamental. Então a produçao industrial vai reagir a tudo isso de maneira defasada. Va" ,dia = (Data 01 09 2020), hora= (PM 10 30),app= WhatsApp}
msg13 = Contato {identificador=(Fone "99445-5700"), mensagem= "i ter que produzir para atender esse pico de demanda e para refazer os estoques, que foram zerados o" ,dia = (Data 01 09 2020), hora= (PM 11 30),app= WhatsApp}
msg14 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "u diminuíram muito durante a pandemia. A produção industrial deveria trabalhar de maneira exagerada," ,dia = (Data 02 09 2020), hora= (AM 0 30),app= LinkedIn}
msg15 = Contato {identificador=(Nome "Denis Costa"), mensagem= " o que chamamos de overshooting. A produção industrial surpreendeu para baixo em agosto, mas não foi" ,dia = (Data 02 09 2020), hora= (AM 0 33),app= Facebook}
msg16 = Contato {identificador=(Fone "99445-5700"), mensagem= "nada dramatico. Na medida em que os setores vão voltando a normalidade, a producao industrial deve" ,dia = (Data 02 09 2020), hora= (AM 4 30),app= WhatsApp}
msg17 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "continuar a retomada nos proximos meses, encontrando seu nivel mais normal la na frente.  Mas entao" ,dia = (Data 02 09 2020), hora= (AM 5 02),app= LinkedIn}
msg18 = Contato {identificador=(Nome "Vinicius Costa"), mensagem= "eh algo pontual essa recuperacao forte? Quando a atividade deve retomar esse ritmo mais normal? Eh dif" ,dia = (Data 02 09 2020), hora= (AM 3 30),app= WhatsApp}
msg19 = Contato {identificador=(Nome "Catalina Lina"), mensagem= "icil dizer porque tem muitas forças que ainda vao bater sobre a economia de ambos os lados, para que" ,dia = (Data 02 09 2020), hora= (AM 8 35),app= Facebook}
msg20 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "continue mais forte e para que desaqueça. O primeiro ponto é o fiscal. Um dos elementos que explica" ,dia = (Data 02 09 2020), hora= (AM 8 50),app= LinkedIn}
msg21 = Contato {identificador=(Nome "Denis Costa"), mensagem= "m a retomada mais forte eh a expansão fiscal enorme que estamos fazendo por conta da pandemia, com de" ,dia = (Data 02 09 2020), hora= (AM 9 00),app= Facebook}
msg22 = Contato {identificador=(Nome "Catalina Lina"), mensagem= "staque para o coronavoucher. Outubro deve ser o pico dos desembolsos de coronavoucher, porque, dado " ,dia = (Data 02 09 2020), hora= (AM 9 05),app= Facebook}
msg23 = Contato {identificador=(Nome "Paula Troiano"), mensagem= "o espaçamento de pagamentos que a Caixa fez, alguns beneficiarios podem receber [no mesmo mes] a par" ,dia = (Data 02 09 2020), hora= (AM 9 15),app= LinkedIn}
msg24 = Contato {identificador=(Nome "Paula Troiano"), mensagem= "cela de R$ 600 e de R$ 300. Enquanto houver dinheiro injetado pelo governo na economia, a demanda br" ,dia = (Data 02 09 2020), hora= (AM 9 26),app= WhatsApp}
msg25 = Contato {identificador=(Nome "Denis Costa"), mensagem= "asileira deve se manter alta. Em dezembro, deve ter queda razoavel de demanda, e, em janeiro, mais a" ,dia = (Data 02 09 2020), hora= (AM 9 32),app= WhatsApp}
msg26 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "inda. Mesmo que se aprove o Renda Cidada, entre R$ 200 e R$ 300, ainda assim deve haver desaceleraça" ,dia = (Data 02 09 2020), hora= (AM 9 45),app= LinkedIn}
msg27 = Contato {identificador=(Nome "Paula Troiano"), mensagem= "o importante, porque o número de beneficiarios deve cair de 64 milhões para algo como 20 milhoes. Do" ,dia = (Data 02 09 2020), hora= (AM 9 50),app= WhatsApp}
msg28 = Contato {identificador=(Nome "Denis Costa"), mensagem= "nosso ponto de vista, deve ter uma desaceleracao forte da demanda em dezembro, janeiro e fevereiro." ,dia = (Data 02 09 2020), hora= (AM 10 30),app= LinkedIn}
msg29 = Contato {identificador=(Nome "Catalina Lina"), mensagem= "E qual sera o ponto positivo para a atividade? A desaceleracao fiscal deve ser concomitante a cheg" ,dia = (Data 02 09 2020), hora= (AM 11 15),app= WhatsApp}
msg30 = Contato {identificador=(Nome "Augusto Costa"), mensagem= "ada da vacina. O Doria [Joao Doria, governador de Sao Paulo] está falando que as primeiras doses che" ,dia = (Data 02 09 2020), hora= (PM 0 45),app= WhatsApp}

-- b) Crie uma estrutura (lista) para armazenar em conjunto os dados das mensagens de texto como as definidas no item a. Crie uma função que ordena a lista de mensagens pelo campo Contato, usando o método de ordenação bolha. Considere que nessa ordenação os números telefônicos devem vir antes de nomes. 

listaMsgs::[Registro]
listaMsgs = [msg1,msg2,msg3,msg4,msg5,msg6,msg7,msg8,msg9,msg10,msg11,msg12,msg13,msg14,msg15,msg16,msg17,msg18,msg19,msg20,msg21,msg22,msg23,msg24,msg25,msg26,msg27,msg28,msg29,msg30]

ordenaListaPorContato :: [Registro] -> [Registro]
ordenaListaPorContato xs = 
  foldr bubble [] xs
  where
    bubble x []     = [x]
    bubble x (y:ys) 
      | (identificador x) < (identificador y)     = x:y:ys
      | otherwise = y:bubble x ys

--c) Crie uma função que ordena a lista de mensagens pela data e hora usando dois o método de ordenação quicksort. 

ordenaListaPorDataHora::[Registro]->[Registro]
ordenaListaPorDataHora [] = []
ordenaListaPorDataHora [x] = [x]
ordenaListaPorDataHora lista =
  let 
    verificaDataHora = (\x y -> (||) ((dia x) < (dia y)) ((&&) ((dia x) <= (dia y))((hora x) < (hora y))))
    separaListas _ [] = ([],[])
    separaListas x (y:ys) 
      | verificaDataHora x y = (listaMenores, y:listaMaiores)
      | otherwise = (y:listaMenores, listaMaiores)
      where (listaMenores, listaMaiores) = (separaListas x ys)
    quickSort [] = []
    quickSort (s:xs) = 
      let 
        (listaMenor, listaMaior) = separaListas s xs
      in (quickSort listaMenor)++[s]++(quickSort listaMaior)
  in quickSort lista

-- d) Defina uma função para consultar as últimas 2 mensagens de um contato qualquer (se houver), postadas em qualquer das redes em questão. Essa função deve receber como entrada: o contato e uma lista de mensagens, que pode estar desordenada. A nova função de consulta deve usar a função do item c para ordenar a lista de entrada. 

buscaUltimasMsgs::Identificador->[Registro]->[Registro]
buscaUltimasMsgs contato lista = 
  let
    listaMsgsDoContato elm lista = filter (\y -> identificador y == elm) lista
    listaOrd = (ordenaListaPorDataHora (listaMsgsDoContato contato lista))
    pegaUltimos (x:xs) 
      | length(x:xs) <3 = (x:xs)
      | otherwise = pegaUltimos xs
  in pegaUltimos listaOrd

-- Exercício 9) Considere o tipo algébrico ArvBinInt visto em sala para representar árvores binárias que armazenam números inteiros. Elabore as funções a seguir que manipulam árvores binárias:

data ArvBinInt =  Nulo | 
                  No Int ArvBinInt ArvBinInt
                  deriving (Show)

arv1 = (No 2 (No 7 (No 2 Nulo Nulo)
        (No 6 (No 5 Nulo Nulo)
        (No 11 Nulo Nulo)))
        (No 5 Nulo
        (No 9 (No 4 Nulo Nulo)
        Nulo))) 

-- a) internos: recebe uma árvore binária e devolve uma listagem com todos os nós internos (não folhas) existentes na árvore.
internos::ArvBinInt->[Int]
internos Nulo = []
internos (No _ Nulo Nulo) = [] --é uma folha
internos (No x esq Nulo) = [x] ++ internos esq --procura na esquerda por mais nós internos
internos (No x Nulo dir) = [x] ++ internos dir --procura na direita por mais nós internos
internos (No x esq dir) = [x] ++ internos esq ++ internos dir 

-- b) somaNos: somar os valores de todos os elementos da árvore binária

somaNos::ArvBinInt->Int
somaNos Nulo = 0
somaNos (No x esq dir) = x + somaNos esq + somaNos dir

-- c) pertence: recebe um valor inteiro e verifica se esse valor é igual a algum dos elementos da árvore binária. 

pertence::Int->ArvBinInt->Bool
pertence x Nulo = False
pertence elem (No x esq dir)
  |x == elem = True
  |otherwise = pertence elem esq || pertence elem dir 

-- Exercício 10) Uma árvore binária pode ser utilizada para armazenar expressões aritméticas. Para isso definimos o tipo ArvBinEA em que uma árvore pode ser vazia, conter um valor numérico ou conter uma expressão com um operador e outras duas expressões: 

data ArvBinEA a = Vazia | Folha a | NoEA (Char, ArvBinEA a, ArvBinEA a)
    deriving (Show)

ea::ArvBinEA Float
ea = NoEA('+', NoEA('*', Folha 10, Folha 5), Folha 7)

-- Faça uma função que receba uma árvore binária de expressão aritmética e retorne o resultado da expressão. Por exemplo, dada a árvore ea desenhada acima, a função deve devolver o valor 57. 

calcula::(Num t, Floating t)=>(ArvBinEA t)->t
calcula (Vazia) = undefined 
calcula (Folha x) = x
calcula (NoEA (y, x, z)) = aplicaOperador y (calcula x) (calcula z)

aplicaOperador::(Num t, Floating t)=>Char->t->t->t
aplicaOperador '*' x y = x*y
aplicaOperador '+' x y = x+y
aplicaOperador '-' x y = x-y
aplicaOperador '/' x y = x / y

main :: IO ()
main = return ()
