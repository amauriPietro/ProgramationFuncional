delta::Float->Float->Float->Float
delta a b c = (b*b) - (4*a*c)

--Ex1
analisa_raizes::Float->Float->Float->String
analisa_raizes a b c
   |a == 0 = "4-equacao degenerada"
   |delta a b c > 0 = "1-possui duas raizes reais"
   |delta a b c == 0 = "2-possui uma raiz real"
   |otherwise = "3-nenhuma raiz real"
   
--Ex2
equacao::Float->Float->Float->(Float, Float)
equacao 0 b c = (c/b, 0)
equacao a b c = ((sqrt(delta a b c) - b)/(2*a), (-sqrt(delta a b c) - b)/(2*a))

--Ex3
type Data = (Int, Int, Int)
calculaIdade::Data->Data->Int
calculaIdade (d, m, a) (dP, mP, aP)
   |mP >= m && dP >= d = a - aP
   |otherwise = (a - aP) - 1

preco::Float->Data->Data->Float
preco total (d, m, a) (dP, mP, aP)
   |calculaIdade(d, m, a) (dP, mP, aP) < 2 = total * 0.15
   |calculaIdade(d, m, a) (dP, mP, aP) < 10 = total * 0.4
   |calculaIdade(d, m, a) (dP, mP, aP) > 70 = total * 0.5
   |calculaIdade(d, m, a) (dP, mP, aP) < 2 = total * 0.15
   |otherwise = total

--Ex4
gera1 = [x^3 | x <- [1..20], x > 2, x < 12]
gera2 = [(x, y) | x <- [1..5], y <- [1..20], y >= x, y <= 3*x]
l1 = [15, 16]

--Ex19
notasTroco::Int->[Int]