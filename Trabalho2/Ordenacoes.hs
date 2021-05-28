--ctrl v compilar
-- :load "C:\\Users\\Amauri Pietropaolo\\Documents\\estudos\\Comp\\haskel\\ProgramationFuncional\\Trabalho2\\Ordenacoes.hs"
--listas para comparação
l1=[1..2000]
l2=[2000,1999..1]
l3=l1++[0]
l4=[0]++l2
l5=l1++[0]++l2
l6=l2++[0]++l1
l7=l2++[0]++l2
x1=[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
x2=[20,19,18,17,16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1]
x3=[11,12,13,14,15,16,17,18,19,20,1,2,3,4,5,6,7,8,9,10]
x4=[10,9,8,7,6,5,4,3,2,1,20,19,18,17,16,15,14,13,12,11]
x5=[11,12,13,14,15,5,4,3,2,1,16,17,18,19,20,10,9,8,7,6]
x6=[1,12,3,14,5,15,4,13,2,11,6,17,8,19,20,10,9,18,7,16]
x7 = [20,8,2,11,13,3,7,18,14,4,16,10,15,1,9,17,19,12,5,6]

--Bubble sort
--Original
bolha::(Ord t)=>[t]->([t], Int)
bolha [] = ([], 0)
bolha lst = bolhaOrd lst (length lst)

bolhaOrd::(Ord t)=>[t]->Int->([t], Int)
bolhaOrd lst 0 = (lst, 0)
bolhaOrd lst n = ((fst(b)) ,(snd(b) + snd(a)))
   where a = troca lst
         b = bolhaOrd (fst(a)) (n-1)

troca::(Ord t)=>[t]->([t], Int)
troca [x] = ([x], 0)
troca (x:y:lst)
   | x > y = (y:fst(a), snd(a) + 1)
   | otherwise = (x:fst(b),snd(b))
   where a = troca(x:lst)
         b = troca(y:lst)

--var1
bolha2::(Ord t)=>[t]->([t], Int)
bolha2 [] = ([], 0)
bolha2 lst = bolhaOrd2 lst (length lst)

bolhaOrd2::(Ord t)=>[t]->Int->([t], Int)
bolhaOrd2 lst 0 = (lst, 0)
bolhaOrd2 lst n = if(snd(a) /= 0) then ((fst(b)) ,(snd(b) + snd(a)))
                 else (lst, 0)
   where a = troca lst
         b = bolhaOrd (fst(a)) (n-1)

--var2
bolha3::(Ord t)=>[t]->([t], Int)
bolha3 [] = ([], 0)
bolha3 lst = bolhaOrd2 lst (length lst)

bolhaOrd3::(Ord t)=>[t]->Int->([t], Int)
bolhaOrd3 lst 0 = (lst, 0)
bolhaOrd3 lst n = if(snd(a) /= 0) then ((fst(b)) ,(snd(b) + snd(a)))
                 else (lst, 0)
   where a = troca lst
         b = bolhaOrd (take (n-1) (fst(a))) (n-1)

--Selection sort
--Original
selection::(Ord t)=>[t]->([t], Int)
selection [] = ([], 0)
selection lst = (a:fst(slct), snd(slct) + (length lst))
                where a = minimum lst
                      slct = selection(rmv lst a)

rmv::(Ord t)=>[t]->t->[t]
rmv (x:lst) n = if(x == n) then lst
                else x:(rmv lst n)
--Selection V1
--selectionV1::(Ord t)=>[t]->([t], Int)
--selectionV1 [] = ([], 0)
--selectionV1 lst = (a:fst(slct), snd(slct) + (length lst))
                     --where slct = selection(rmv_menor lst)

--rmv_menor::(Ord t)=>[t]->([t], Int)
--rmv_menor [x] = ([x], 0)

--Selection V2
selectionv2::(Ord t)=>[t]->([t], Int)
selectionv2 [] = ([], 0)
selectionv2 lst = (a:fst(slct), snd(slct) + (length lst))
                where a = foldr1 min lst
                      slct = selection(rmv lst a)

--Insertion Sort
--Original
insertion::(Ord t)=>[t]->([t], Int)
insertion [] = ([], 0)
insertion (x:lst) = (fst(insout), snd(ins) + snd(insout))
                    where ins = insertion lst
                          insout = insere (fst(ins)) x

insere::(Ord t)=>[t]->t->([t], Int)
insere [] n = ([n], 0)
insere (x:lst) n = if(n <= x) then (n:x:lst, 1)
                 else (x:fst(ins), snd(ins) + 1)
                 where ins = insere lst n

--InsertionV2
insertionv2::[Int]->[Int]
insertionv2 [] = []
insertionv2 lst = foldr (insere2) [] lst

insere2::Int->[Int]->[Int]
insere2 n [] = [n]
insere2 n (x:lst) = if(n <= x) then n:x:lst
                 else x:(insere2 n lst)
--Quicksort
--Original
qsort::(Ord t)=>[t]->([t], Int)
qsort [] = ([], 0)
qsort (p:lst) = (fst(lst1) ++ [p] ++ fst(lst2), snd(lst1) + snd(lst2) + (2 * length lst))
   where lst1 = qsort [y | y <- lst, y < p]
         lst2 = qsort [y | y <- lst, y >= p]
--Variação 1
divide::(Ord t)=>[t]->t->([t],[t])
divide [] n = ([], [])
divide (x:lst) n
   |x < n = ((x:fst(dvd)), snd(dvd))
   |x >= n = (fst(dvd), (x:snd(dvd)))
   |otherwise = (fst(dvd), snd(dvd))
   where dvd = divide lst n

qsortVar1::(Ord t)=>[t]->([t], Int)
qsortVar1 [] = ([], 0)
qsortVar1 (p:lst) = (fst(qs1) ++ [p] ++ fst(qs2), length(p:lst) + snd(qs1) + snd(qs2))
   where listas = divide lst p
         qs1 = qsortVar1(fst(listas))
         qs2 = qsortVar1(snd(listas))

--qsortVar2
meio::(Ord t)=>t->t->t->t
meio a b c
   |a > b && c > a = a
   |b > a && c > b = b
   |otherwise = c

qsortVar2::(Ord t)=>[t]->([t], Int)
qsortVar2 [] = ([], 0)
qsortVar2 [x] = ([x], 0)
qsortVar2 (x:y:[]) = if(x > y) then ((y:x:[]), 1)
                     else ((x:y:[]), 1)
qsortVar2 (p:q:s:lst) = (fst(qs1) ++ [pmed] ++ fst(qs2), length(p:lst) + snd(qs1) + snd(qs2))
   where pmed = meio p q s
         listas = divide lst pmed
         qs1 = qsortVar1(fst(listas))
         qs2 = qsortVar1(snd(listas))




--Mergesort
--Original
mergesort::(Ord t)=>[t]->([t], Int)
mergesort [] = ([], 0)
mergesort [x] = ([x], 0)
mergesort lst = (fst(interc), snd(merge1) + snd(merge2) + snd(interc))
                 where metade = div(length(lst)) 2
                       merge1 = mergesort(take metade lst)
                       merge2 = mergesort(drop metade lst)
                       interc = intercala (fst(merge1)) (fst(merge2))

intercala::(Ord t)=>[t]->[t]->([t], Int)
intercala [] lst = (lst, 0)
intercala lst [] = (lst, 0)
intercala (x:lst1) (y:lst2) = if (x > y) then (y:(fst(interc1)), snd(interc1) + 1)
                          else (x:(fst(interc2)), snd(interc2) + 1)
                          where interc1 = intercala (x:lst1) lst2
                                interc2 = intercala lst1 (y:lst2)