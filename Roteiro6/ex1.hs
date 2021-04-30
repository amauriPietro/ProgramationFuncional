--a) where
type Data = (Int, Int, Int)
valida::Data->Bool
valida (d, m, a)
   | d <= 31 && d >= 1 && (m == 1 || m == 3 || m == 5 || m == 7 ||m == 8 || m == 10 ||m == 12) = True
   | d <= 30 && d >= 1 && (m == 4 || m == 6 || m == 9 || m == 11) = True
   | bissexto a == True && m == 2 && d <= 29 && d >= 1 = True
   | m == 2 && d <= 28 && d >= 1 = True
   | otherwise = False
   where
      bissexto x = if(mod x 400 == 0 || mod x 4 == 0 && mod x 100 /= 0) then True
                   else False

--a) let
let (x = if(mod x 400 == 0 || mod x 4 == 0 && mod x 100 /= 0) then True
                 else False)
in valida::Data->Bool
valida (d, m, a)
   | d <= 31 && d >= 1 && (m == 1 || m == 3 || m == 5 || m == 7 ||m == 8 || m == 10 ||m == 12) = True
   | d <= 30 && d >= 1 && (m == 4 || m == 6 || m == 9 || m == 11) = True
   | bissexto a == True && m == 2 && d <= 29 && d >= 1 = True
   | m == 2 && d <= 28 && d >= 1 = True
   | otherwise = False



--2
validaBissextos::[Int]->[Int]
validaBissextos lst = [x | x <- lst, bissexto(x) == True]
   where
      bissexto x = if(mod x 400 == 0 || mod x 4 == 0 && mod x 100 /= 0) then True
                   else False

--3
