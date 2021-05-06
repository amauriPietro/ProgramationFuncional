--Ex1
ehPar::Int->Bool
ehPar x = if(mod x 2 == 0) then True
              else False

paridade::[Int]->[Bool]
paridade xs = map ehPar xs

--Ex2
prefixos::[String]->[String]
prefixos xs = map (take 3) xs

--Ex3
saudacao::[String]->[String]
saudacao xs = map ("Oi " ++) xs

--Ex4
filtrar::(a->Bool) -> [a] -> [a]
filtrar p xs = [x | x <- xs, p x == True]

--Ex5
pares::[Int]->[Int]
pares xs = filter ehPar xs

--Ex6
solucoes::[Int]->[Int]
solucoes l = filter (\x->((5*x)+6) < (x*x)) l

--Ex7
maior::[Int]->Int
maior xs = foldr1 max xs

--Ex8
menor_min10::[Int]->Int
menor_min10 xs = foldr min 10 xs

--Ex9
junta_silabasplural::[String]->String
junta_silabasplural xs = foldr (++) "s" xs

--Ex10
menores10::[Int]->([Int],Int)
menores10 xs = (lst, length(lst))
          where lst = filter (<10) xs

--Ex11
busca_elem::Int->[Int]->(Bool, Int)
busca_elem _ [] = (False, 0)
busca_elem n (x:xs) = if(x == n) then (True, 1)
                    else (False || fst(busca_elem n xs), 1 + snd(busca_elem n xs))