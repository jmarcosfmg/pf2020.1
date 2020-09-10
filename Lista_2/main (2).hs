--1) Reescreva as funções do último roteiro (dobro, quadruplicar, hipotenusa, distância), definindo a prototipação de tipos de cada função. Obs: se você já fez isso no primeiro roteiro, envie novamente para o exercício ficar completo.
dobro::Double->Double
dobro x = 2 * x

quadruplicar::Double->Double
quadruplicar x = (dobro x) + (dobro x)

hipotenusa::Double->Double->Double
hipotenusa cat1 cat2 = sqrt(cat1^2 + cat2^2)

type Ponto = (Double, Double)
distancia::Ponto->Ponto->Double
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

--2) Print no email

--3) Dado um valor monetário em reais, escreva uma função conversao que retorna uma tupla-3 com o valor em Real, Dolar e Euro, sendo que 1 real = 3,96 dólares = 4,45 euros

converte::Double->(Double,Double,Double)
converte real = (real, real*3.96, real*4.45)

-- 4) Implemente a função bissexto que, dado um ano (inteiro), indique se ele é bissexto ou não.

bissexto:: Int-> Bool
bissexto x | (mod x 400 == 0) = True
  | (mod x 4 == 0) && (mod x 100 /= 0) = True
  | otherwise = False

-- 5) Defina o tipo Data dado a seguir. Escreva a função bissexto2 que recebe uma data e indique se ela pertence a um ano bissexto ou não.

type Data = (Int, Int, Int)
bissexto2::Data->Bool
bissexto2 (dia,mes,ano) | (mod ano 400 == 0) = True
  | (mod ano 4 == 0) && (mod ano 100 /= 0) = True
  | otherwise = False

--6) Escreva a função valida que indica se uma data é válida ou não.
valida::Data->Bool
valida (dia,mes,ano)
  | dia >= 1 && dia <= 31 && (mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) = True
  | dia >= 1 && dia <= 30 && (mes == 4 || mes == 6 || mes == 9 || mes == 11) = True
  | dia >= 1 && dia <= 28 && mes == 2 && not (bissexto ano) = True
  | dia >= 1 && dia <= 29 && mes == 2 && (bissexto ano) = True
  | otherwise = False

--7) Escreva a função precede que recebe 2 datas e indica se a 1a data é anterior à 2a.
precede::Data->Data->Bool
precede (d1,m1,a1) (d2,m2,a2)
  | a1 < a2 = True
  | a1 > a2 = False
  | m1 < m2 = True
  | m2 > m1 = False
  | d1 < d2 = True
  | d1 > d2 = False
  | otherwise = False

--8) Implemente as estruturas de dados (tuplas) para um sistema de gerenciamento de bibliotecas e depois as defina como tipos. O sistema tem 3 estruturas básicas:
-- Livro: composto por código do livro, título do livro, autor, editora e ano de publicação.
--Aluno: composto por código do aluno, nome, e-mail e telefone.
-- Empréstimo: composto por código do livro, código do aluno, data de empréstimo, data de devolução e situação. Obs: utilize a estrutura/tipo auxiliar data do exercício 4.

type Livro = (String, String, String,String, Data)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)

--9) Seja o tipo Emprestimo e o exemplo dado a seguir, composto por código do livro, código do aluno, data de empréstimo, data de devolução e situação. Escreva uma função que verifica se um empréstimo está em dia, dado um empréstimo e a data de hoje.

e1::Emprestimo
e1 = ("H123C9","BSI200945",(12,9,2009),(20,9,2009),"aberto")

emprestimo_em_dia::Emprestimo->Data->Bool
emprestimo_em_dia (_, _, _, data_devolucao, situacao) data_atual 
  | situacao == "fechado" = True
  | data_devolucao == data_atual = True
  | precede data_atual data_devolucao = True
  | otherwise = False

main :: IO ()
main = return ()