--Ex1
--a)
ouc1::Bool->Bool->Bool
ouc1 True True = True
ouc1 True False = True
ouc1 False True = True
ouc1 False False = False

ouc2::Bool->Bool->Bool
ouc2 False b = b
ouc2 True _ = True

ouc3::Bool->Bool->Bool
ouc3 False False = False
ouc3 _ _ = True

--b)
oug1::Bool->Bool->Bool
oug1 a b
   |a /= b = True
   |a == False = False
   |otherwise = True

oug2::Bool->Bool->Bool
oug2 a b
   |a == True = True
   |b == False = False
   |otherwise = True
   
--Ex2
hipot::Float->Float->Float
hipot cato cata = sqrt ((cato * cato) + (cata * cata))
dist::Float->Float->Float->Float->Float
dist x1 y1 x2 y2 = hipot (x2 - x1) (y2 - y1)

--Ex3
fatg::Int->Int
fatg n
   |n > 0 = n * fatg(n-1)
   |n == 0 = 1
   |otherwise = -1
   
fatc::Int->Int
fatc 0 = 1
fatc n = n * fatc(n-1)

--Ex 4
fibo::Int->Int
fibo n
   |n == 1 = 1
   |n == 2 = 1
   |n > 2 = fibo(n-1) + fibo(n-2)
   |otherwise = -1 --Evitar numeros negativos
   
--Ex5
n_tri::Int->Int
n_tri 0 = 0
n_tri n = (n-1) + n_tri(n-1)

--Ex6
potencia2::Int->Int
potencia2 0 = 1
potencia2 n = 2 * potencia2(n-1)

--Ex7
--a)
prodIntervalo::(Int, Int)->Int
prodIntervalo (m, n)
   |m == n = n
   |otherwise = n * prodIntervalo (m, n-1)

--b)
fatprod::Int->Int
fatprod n = prodIntervalo(1, n)

--Ex8
resto_div::(Int, Int)->Int
resto_div (m, n)
   |m < n = m
   |otherwise = resto_div(m-n, n)

divAux::(Int, Int, Int)->(Int, Int, Int)
divAux(cnt, m, n)
   |m < n = (cnt, m, n)
   |otherwise = divAux(cnt+1, m-n, n)   

auxtodiv::(Int, Int, Int)->Int
auxtodiv(a, b, c) = a

div_inteira::(Int, Int)->Int
div_inteira(m, n) = auxtodiv(divAux(0, m, n))

--Ex9
--guarda
mdcg::(Int, Int)->Int
mdcg(m, n)
   |n == 0 = m
   |otherwise = mdcg(n, (mod m n))

--casamento
mdcc::(Int, Int)->Int
mdcc(m, 0) = m
mdcc(m, n) = mdcc(n, (mod m n))

--Ex10
--guarda
binomialg::(Int, Int)->Int
binomialg(n, k)
   |k == 0 = 1
   |k == n = 1
   |otherwise = binomialg(n-1, k) + binomialg(n-1, k-1)

--casamento
binomialc::(Int, Int)->Int
binomialc(n, 0) = 1
binomialc(n, k) = if(k == n)
   then 1
   else binomialc(n-1, k) + binomialc(n-1, k-1)

--Ex11
passo::(Int, Int)->(Int, Int)
passo(a, b) = (b, (a+b))

fiboAux::(Int, Int, Int)->(Int, Int, Int)
fiboAux(1, a, _) = (1, a, 0)
fiboAux(n, a, b) = fiboAux(n-1, fst(passo(a, b)), snd(passo(a, b)))

auxtofibo::(Int, Int, Int)->Int
auxtofibo(a, b, c) = b

fibo2::Int->Int
fibo2 n = auxtofibo(fiboAux(n, 1, 1))

