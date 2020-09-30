-- 1) Refaça as seguintes funções dos roteiros 2, 3 e 4, utilizando o comando
-- “where” para definições locais (incluindo funções auxiliares que são
-- necessárias na solução da função principal): 


--a) Escreva a função valida que indica se uma data é válida ou não. 

type Data = (Int, Int, Int)

valida::Data->Bool
valida (dia,mes,ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False
  where
  bissexto ano
    | (mod ano 400 == 0) = True
    | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
    | otherwise = False

-- b) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma lista com os valores que representam anos bissextos.

bissextos::[Int]->[Int]
bissextos lista 
  | lista == [] = []
  | eh_bissexto x = x: (bissextos xs)
  | otherwise = bissextos xs
  where 
      (x:xs) = lista
      eh_bissexto ano
        | (mod ano 400 == 0) = True
        | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
        | otherwise = False

-- c) Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados.

type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados::Emprestimos->Data->Emprestimos
atrasados listaEmprestados data_atual  
  | listaEmprestados == [] = []
  | (precede data_devolucao data_atual) && estado == "aberto" = x:(atrasados xs data_atual)
  | otherwise = atrasados xs data_atual
  where 
     (x:xs) = listaEmprestados
     (_,_,_,data_devolucao,estado) = x
     precede (d1,m1,a1) (d2,m2,a2)
        | a1 < a2 = True
        | a1 > a2 = False
        | m1 < m2 = True
        | m2 > m1 = False
        | d1 < d2 = True
        | d1 > d2 = False
        | otherwise = False

-- d) Faça uma segunda definição da função recursiva fibo2 que retorna
-- o n-ésimo termo da sequência de Fibonacci utilizando recursividade
-- e os conceitos a seguir (use a função passo(x,y)).

fibo2::Int->Int
fibo2 n = x 
  where 
    (x,_) = fibo n
    passo(x,y) = (y, x+y)
    fibo k 
     | k == 0 = (0,1)
     | otherwise = passo(fibo(k-1))

e) Escreva a função fatorial usando a função prodIntervalo.

fatorial::Int->Int
fatorial n = prodIntervalo [1..n]
  where
    prodIntervalo [] = 1
    prodIntervalo(x:xs) = x*(prodIntervalo xs)

-----------------------------------------------------------------------------------------------------------------------------

-- 2) Refaça as funções do exercício 1, utilizando o comando “let” para
-- definições locais (incluindo funções auxiliares que são necessárias na
-- solução da função principal). Repetir para os itens “a” a “e”. 


--a) Escreva a função valida que indica se uma data é válida ou não. 

valida::Data->Bool
valida (dia,mes,ano) = 
  let 
    bissexto ano
      | (mod ano 400 == 0) = True
      | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
      | otherwise = False
    verifica_data (dia, mes, ano)
      | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
      | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
      | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
      | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
      | otherwise = False
  in 
    verifica_data (dia, mes, ano)
	
-- b) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma lista com os valores que representam anos bissextos.

bissextos::[Int]->[Int]
bissextos lista =
  let 
    eh_bissexto ano
      | (mod ano 400 == 0) = True
      | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
      | otherwise = False
  in [x | x <- lista, eh_bissexto x]
	
-- c) Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados.

type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

atrasados::Emprestimos->Data->Emprestimos
atrasados listaEmprestados data_atual = 
  let
    precede (d1,m1,a1) (d2,m2,a2)
      | a1 < a2 = True
      | a1 > a2 = False
      | m1 < m2 = True
      | m2 > m1 = False
      | d1 < d2 = True
      | d1 > d2 = False
      | otherwise = False
  in [x | x<- listaEmprestados, let (_,_,_,data_devolucao,estado) = x, (precede data_devolucao data_atual) && estado == "aberto" ] 

-- d) Faça uma segunda definição da função recursiva fibo2 que retorna
-- o n-ésimo termo da sequência de Fibonacci utilizando recursividade
-- e os conceitos a seguir (use a função passo(x,y)).

fibo2::Int->Int
fibo2 n =
  let
    passo(x,y) = (y, x+y)
    fibo k 
      | k == 0 = (0,1)
      | otherwise = passo(fibo(k-1))
    primeiro (x,y) = x
  in primeiro(fibo n) 
 
e) Escreva a função fatorial usando a função prodIntervalo.

fatorial::Int->Int
fatorial n = 
  let 
    prodIntervalo [] = 1
    prodIntervalo(x:xs) = x*(prodIntervalo xs)
    listar 0 = []
    listar n = n:(listar(n-1))
  in prodIntervalo(listar n)
  
-------------------------------------------------------------------------------------------------------------------------------------

-- 3) Aplicar Beta-redução nas expressões lambda a seguir: 

-- 3.1(\x. 2*x + 1) 3
-- (\x. 2*x + 1) 3
-- 2*3 + 1
-- 6+1
-- 7

-------------------------------------------------------
-- 3.2 (\xy. x-y) 5 7
-- (\xy. x-y) 5 7
-- 5-7
-- -2

-------------------------------------------------------
-- 3.3 (\yx. x-y) 5 7
-- (\yx. x-y) 5 7
-- 7-5
-- 2

-------------------------------------------------------
-- 3.4 (\xy. x-y) (\z. z/2)
-- (\xy. x-y) (\z. z/2)
-- (\y. z/2 - y)
-- Não exite redução possível pois não existem valores suficientes que possam ser aplicados

-------------------------------------------------------
-- 3.5 (\xy. x-y)((\z.z/2) 6) 1
-- (\xy. x-y)((\z.z/2) 6) 1
-- (\xy. x-y)((\z.z/2) 6) 1
-- (\xy. x-y)(6/2) 1
-- (\xy. x-y) 3 1
-- (3 - 1)
-- 2

-------------------------------------------------------
-- 3.6 (\x.\y. - x y) 9 4
-- (\x.\y. - x y) 9 4
-- (\y. - 9 y) 4
-- (- 9 4)
-- 5

-------------------------------------------------------
-- 3.7 (\x.xx)(\y. y)
-- (\x.xx)(\y. y)
-- (\y. y)(\y. y)
-- (\y. y)
-- Não exite redução possível pois não existem valores suficientes que possam ser aplicados

main :: IO ()
main = return ()