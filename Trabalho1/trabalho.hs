import Data.List(delete)

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
gera3 = [x | y <- l1, x <- [1..20], x <= y]
gera4 = [(x, y) | x <- [1..20], y <- [1..10], y == (x + 1), mod x 2 == 0]
gera5 = [x + y | (f, s) <- gera4, x <- [1..20], y <- [1..20], x == f, y == s]

--Ex5
--a)
contaNegM2::[Int]->Int
contaNegM2 lst = length([x | x <- lst, x > 0, mod x 3 == 0])

--b)
listaNegM2::[Int]->[Int]
listaNegM2 lst = [x | x <- lst, x > 0, mod x 3 == 0]

--Ex6
fatores::Int->[Int]
fatores num = [x | x <- [1..num], mod num x == 0]

primos::Int->Int->[Int]
primos a b = [x | x <- [a..b], fatores x == [1, x]]

--Ex7
mmc::Int->Int->Int->Int
mmc a b c = head [x | x <- [a..a*b*c], mod x a == 0, mod x b == 0, mod x c == 0]

--Ex8
serie::Float->Int->Float
serie x 0 = 0
serie x n = if (mod n 2) == 0 then x/fromIntegral(n) + serie x (n-1)
                            else fromIntegral(n)/x + serie x (n-1)

--Ex9
fizzbuzz::Int->[String]
fizzbuzz n = reverse(auxfizzbuzz n)

auxfizzbuzz::Int->[String]
auxfizzbuzz n
   |n == 0 = []
   |mod n 6 == 0 = "FizzBuzz":(auxfizzbuzz (n-1))
   |mod n 2 == 0 = "Fizz":(auxfizzbuzz (n-1))
   |mod n 3 == 0 = "Buzz":(auxfizzbuzz (n-1))
   |otherwise = "No":(auxfizzbuzz (n-1))

--Ex10
seleciona_multiplos::Int->[Int]->[Int]
seleciona_multiplos n lst = [x | x <- lst, mod x n == 0]

--Ex11
unica_ocorrencia::Int->[Int]->Bool
unica_ocorrencia n lst
   |length([x | x <-lst, x == n]) == 1 = True
   |otherwise = False

--Ex12
intercala::[k]->[k]->[k]
intercala [] x = x
intercala (x:r) y = x:(intercala y r)

--Ex13
zipar::[k]->[k]->[[k]]
zipar [] _ = []
zipar _ [] = []
zipar (x:r) (y:s) = ([x, y]):(zipar r s)

--Ex14
type Agenda = (String, String, String, String)
recuperaContato::String->[Agenda]->String
recuperaContato _ [] = "Email desconhecido"
recuperaContato busca ((nome, end, tel, email):r) = if busca == email then nome
                                                    else recuperaContato busca r
--Ex15
type Pessoa = (String, Float, Int, Char)
pessoas::[Pessoa]
pessoas = [("Rosa", 1.66, 27,'F'),("João", 1.85, 26, 'C'),("Maria", 1.55, 62, 'S'),("Jose", 1.78, 42, 'C'),("Paulo", 1.93, 25, 'S'),("Clara", 1.70, 33, 'C'),("Bob", 1.45, 21, 'C'),("Rosana", 1.58,39, 'S'),("Daniel", 1.74, 72, 'S'),("Jocileide", 1.69, 18, 'S')]


mediaAlt::[Pessoa]->Float
mediaAlt [] = 0
mediaAlt ((nome, alt, idd, s):r) = if length(r) + 1 == length pessoas then (alt + mediaAlt r)/fromIntegral(length pessoas)
                                   else alt + mediaAlt r

maisNova::[Pessoa]->Int
maisNova [(nome, alt, idd, s)] = idd
maisNova ((nome, alt, idd, s):r) = if mnv > idd then idd
                                   else mnv
                                   where mnv = maisNova r

nomEst::[Pessoa]->(String, Char)
nomEst [(nome, alt, idd, s)] = (nome, s)
nomEst ((nome, alt, idd, s):(nome1, alt1, idd1, s1):r) = if idd > idd1 then nomEst((nome, alt, idd, s):r)
                                                       else nomEst((nome1, alt1, idd1, s1):r)

maior50::[Pessoa]->[Pessoa]
maior50 [] = []
maior50 ((nome, alt, idd, s):r) = if idd >= 50 then ((nome, alt, idd, s):maior50 r)
                                  else maior50 r

maiorI::Int->[Pessoa]->Int
maiorI _ [] = 0
maiorI i ((nome, alt, idd, s):r) = if (idd > i && s == 'C') then 1 + maiorI i r
                                   else maiorI i r

--Ex16
insere_ord ::Ord t=>t->[t]->[t]
insere_ord n [] = [n]
insere_ord n (x:lst) = if n > x then (x:(insere_ord n lst))
                                else n:(x:lst)

--Ex17
reverte::[t]->[t]
reverte [] = []
reverte (x:lst) = (reverte lst)++[x]

--Ex18
elimina_repet::Eq t=>[t]->[t]
elimina_repet [] = []
elimina_repet (x:lst) = if elem x lst then elimina_repet lst
                                      else (x:elimina_repet lst)
--Ex19
disponiveis = [1,2,5,10,20,50,100]
notasTroco::Int->[[Int]]
notasTroco n = if n <= 0 then [[]]
               else [x:r | x <- disponiveis, r <- notasTroco(n-x), x <= n]

--Ex20
nDamas::Int->[[Int]]
nDamas n = perm [1..n]

perm::[Int] -> [[Int]]
perm [] = [[]]
perm lst = [ x:r | x <- lst, r <- perm (delete x lst), checa x (length(x:r)) r == True]

checa::Int->Int->[Int]->Bool
checa _ _ [] = True
checa lin col (x:lst) = if abs (x - lin) == abs (length(x:lst) - col) then False
                        else checa lin col lst