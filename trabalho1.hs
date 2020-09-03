triangulo::Double->Double->Double->String
triangulo a b c
  | (a+b+c) /= 180.0 || a <= 0 || b <= 0 || c <= 0 = "nao_triangulo"
  | a == b && b == c = "equilatero"
  | a > 90 || b > 90 || c > 90 = "obtuso"
  | a == 90 || b == 90 || c == 90 = "retangulo"
  | otherwise = "simples"


equacao::Double->Double->Double->(Double,Double)
equacao a b c
  | a == 0.0 = ((-c)/b, a)
  | otherwise = (raiz1, raiz2)
  where
      delta = b*b -4*a*c;
      raiz1 = (((-b) + (sqrt(delta)))/(2*a));
      raiz2 = (((-b) - (sqrt(delta)))/(2*a))

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

main :: IO ()
main = return ()
