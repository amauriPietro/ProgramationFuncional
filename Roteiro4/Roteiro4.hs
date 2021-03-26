--Ex2
listaa = [5,4..1]

listab = ['a','c'..'e']

listac = [1,4..16]

listad = zip [1,(-2)..(-11)] [1,5..17]

--Ex3
--a)
intervalo::Int->Int->[Int]
intervalo a b = [a..b]

--b)
intPar::Int->Int->[Int]
intPar a b
   |mod a 2 == 0 = [a+2, a+4..b-1]
   |otherwise = [a+1,a+3..b-1]
   
--Ex5
quadrados::Int->Int->[Int]
quadrados a b = [x^2 | x <- [a..b]]

--Ex6
seleciona_impares::[Int]->[Int]
seleciona_impares a = [x | x <- a, odd x]

--Ex7
tabuada::Int->[Int]
tabuada a = [x * a | x <- [1..10]]

--Ex8
bissexto::Int->Bool
bissexto x
   | mod x 400 == 0 = True
   | mod x 4 == 0 && mod x 100 /= 0 = True
   | otherwise = False
   
bissextos::[Int]->[Int]
bissextos a = [x | x <- a, bissexto x == True]

--Ex9
sublistas::[[Int]]->[Int]
sublistas a = concat a

--Ex10
type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]
bdEmprestimo::Emprestimos
bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]
verifica::Emprestimo->Data->Bool
verifica (codL, codA, (empd, empm, empa), (devd, devm, deva), sit) (d, m, a)
-- situação "encerrado" conta como atraso?
--   | sit == "encerrado" = True
   |(a, m, d) >= (deva, devm, devd) = True
   | otherwise = False

atrasados::Emprestimos->Data->Emprestimos
atrasados a b = [x | x <- a, (verifica x b) == True]

--Ex11
uniaoNRec::[Int]->[Int]->[Int]
uniaoNRec a b = a ++ [x | x <- b, not(elem x a)]