-- 2) Escreva uma função para calcular o dobro de um número.
dobro x = 2 * x
-- 3) Escreva uma função para quadruplicar um número usando a função dobro definida no item anterior.
quadruplicar x = (dobro x) + (dobro x)
-- 4) Escreva uma função que, dadas as medidas dos catetos de um triângulo retângulo, retorne o valor de sua hipotenusa.
hipotenusa cat1 cat2 = sqrt(cat1^2 + cat2^2)
--5) Escreva uma função para calcular a distância entre dois pontos A e B num plano cartesiano.
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)


main :: IO ()
main = return ()