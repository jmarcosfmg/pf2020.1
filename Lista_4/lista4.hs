--2) Escreva a função quadrados que recebe dois inteiros e retorna os quadrados dos
--números entre eles.
quadrados::Int->Int->[Int]
quadrados a b = [x*x | x <- [a, b+1]]
--3) Usando lista por compreensão, escreva a função seleciona_ímpares que recebe um
--lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na lista
--de entrada
seleciona_impares::[Int]->[Int]
seleciona_impares lista = [x | x <- lista, mod x 2 /= 0]
--4) --Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez
--primeiros múltiplos.
tabuada::Int->[Int]
tabuada n = [k*n | k <-[1..10]]
--5)--Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma
--lista com os valores que representam anos bissextos. Dica: use a função bissexto do
--roteiro anterior.

bissexto:: Int-> Bool
bissexto x | (mod x 400 == 0) = True
  | (mod x 4 == 0) && (mod x 100 /= 0) = True
  | otherwise = False

bissextos::[Int]->[Int]
bissextos lista = [ano | ano <- lista, bissexto ano]

-- 6)Usando lista por compreensão, escreva a função sublistas que recebe uma lista
-- formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da
-- lista de entrada na mesma ordem, mas no nível da lista principal, sem sublistas.
sublistas::[[a]]->[a]
sublistas listaDlistas = [y | x <- listaDlistas, y<-x]

-- 7) Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do exemplo
--da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos
--e a Data atual, e retorna uma lista com todos os empréstimos atrasados. 
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2)
  | a1 < a2 = True
  | a1 > a2 = False
  | m1 < m2 = True
  | m2 > m1 = False
  | d1 < d2 = True
  | d1 > d2 = False
  | otherwise = False

atrasados::Emprestimos->Data->Emprestimos
atrasados listaEmprestados data_atual = [emprestimo | emprestimo<-listaEmprestados, let (_,_,_,data_entrega,_) = emprestimo, (precede data_entrega data_atual) == True]

-- 8) Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a
-- quantidade de números pares pertencentes à lista.
npares::[Int]->Int
npares lista= length([x| x<-lista, mod x 2 == 0])

-- 9) Escreva a função recursiva produtorio que recebe uma lista de números e retorna o
-- produto de todos os seus elementos.

produtorio::Num a =>[a]->a
produtorio [] = 0
produtorio (x:xs) = if length(xs) == 0 then x 
                                else x * produtorio xs

-- 10) Escreva a função recursiva comprime a seguir que recebe uma lista de listas e retorna
-- uma lista contendo todos os elementos das sublistas.

comprime::[[a]]->[a]
comprime [] = []
comprime (x:xs) = x++(comprime xs)

--11) Escreva a função tamanho a seguir que recebe uma lista polimórfica (de qualquer
--tipo) e retorna a quantidade de elementos que ela possui.

tamanho::[a]->Int
tamanho [] = 0
tamanho (x:xs) = 1 + (tamanho xs)

--12) Usando compreensão de listas, escreva a função uniaoNRec a seguir que faz a união
--de duas listas de modo que ela mantenha todos os elementos da 1a lista na mesma
--ordem e no final acrescenta apenas os elementos da 2a lista que não estejam presentes
--na 1a lista.
uniaoNRec::Eq a => [a]->[a]->[a]
uniaoNRec lista1 lista2 = lista1++[y | y<-lista2, (elem y lista1) == False]

--13) Escreva a função recursiva uniaoRec2 a seguir que faz a união de duas listas de
--modo que ela mantenha todos os elementos da 1a lista na mesma ordem e no final
--acrescenta apenas os elementos da 2a lista que não estejam presentes na 1a lista.

uniaoRec2::Eq a =>[a]->[a]->[a]
uniaoRec2 lista1 [] = lista1
uniaoRec2 lista1 (x:xs) 
   | elem x lista1  == True = uniaoRec2 lista1 xs
   | otherwise =  (uniaoRec2 (lista1++[x]) xs)

main :: IO ()
main = return ()
