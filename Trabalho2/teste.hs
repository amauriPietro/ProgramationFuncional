insertion::[Int]->[Int]
insertion [] = []
insertion lst = foldr (insere) [] lst
--sort xs = foldr (insert) [] xs

insere::Int->[Int]->[Int]
insere n [] = [n]
insere n (x:lst) = if(n <= x) then n:x:lst
                 else x:(insere n lst)