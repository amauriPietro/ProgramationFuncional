--EX 2
dobro::Int->Int
dobro x = 2 * x
quadruplo::Int->Int
quadruplo x = 2 * dobro x
hipot::Float->Float->Float
hipot cato cata = sqrt ((cato * cato) + (cata * cata))
dist::Float->Float->Float->Float->Float
dist x1 y1 x2 y2 = hipot (x2 - x1) (y2 - y1)

--EX 3
converte::Float->(Float, Float, Float)
converte x = (x, 3.96 * x, 4.45 * x)

--EX 4
bissexto::Int->Bool
bissexto x
   | mod x 400 == 0 = True
   | mod x 4 == 0 && mod x 100 /= 0 = True
   | otherwise = False

--EX 5
type Data = (Int, Int, Int)
bissexto2::Data->Bool
bissexto2 (d, m, a)
   | mod a 400 == 0 = True
   | mod a 4 == 0 && mod a 100 /= 0 = True
   | otherwise = False

--EX 6
valida::Data->Bool
valida (d, m, a)
   | d <= 31 && d >= 1 && (m == 1 || m == 3 || m == 5 || m == 7 ||m == 8 || m == 10 ||m == 12) = True
   | d <= 30 && d >= 1 && (m == 4 || m == 6 || m == 9 || m == 11) = True
   | bissexto2(d, m, a) == True && m == 2 && d <= 29 && d >= 1 = True
   | m == 2 && d <= 28 && d >= 1 = True
   | otherwise = False

--EX 7
precede::Data->Data->Bool
precede (d1, m1, a1) (d2, m2, a2)
   | (d2 == 1) && (d1 == 31) && (m1 == 1 || m1 == 3 || m1 == 5 || m1 == 7 ||m1 == 8 || m1 == 10) && (m2 == m1 + 1) && (a1 == a2) = True
   | (d2 == 1) && (d1 == 30) && (m1 == 4 || m1 == 6 || m1 == 9 || m1 == 11) && (m2 == m1 + 1) && (a1 == a2) = True
   | (d2 == 1) && (d1 == 31) && (m1 == 12) && (m2 == 1) && (a2 == a1 + 1) = True
   | (m1 == 2) && (bissexto2 (d1, m1, a1) == True) && (d1 == 29) && (d2 == 1) && (m2 == 3) && (a1 == a2) = True
   | (m1 == 2) && (bissexto2 (d1, m1, a1) == False) && (d1 == 28) && (d2 == 1) && (m2 == 3) && (a1 == a2) = True
   | (d1 == d2 - 1) && (m1 == m2) && (a1 == a2) = True
   |otherwise = False

--EX 8
type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)

--EX 9
verifica::Emprestimo->Data->String
verifica (codL, codA, (empd, empm, empa), (devd, devm, deva), sit) (d, m, a)
   | sit == "fechado" = "Em dia!"
   |(a, m, d) <= (deva, devm, devd) = "Em dia!"
   | otherwise = "Devendo!"