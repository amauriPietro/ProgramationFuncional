--Ex1
conta_ch::[Char]->Int
conta_ch [] = 0
conta_ch (_:r) = 1 + conta_ch r

conta::[t]->Int
conta [] = 0
conta (_:r) = 1 + conta r

maior::[Int]->Int
maior [x] = x
maior (x:y:r) = if x > y then maior (x:r)
                         else maior (y:r)

primeiros::Int->[Int]->[Int]
primeiros 0 _ = []
primeiros _ [] = []
primeiros q (x:r) = x:(primeiros (q-1) r)

pertence::Eq t=>t->[t]->Bool
pertence a [] = False
pertence a (x:r) = if (x == a) then True
                               else pertence a r 

uniaoR::Eq t=>[t]->[t]->[t]
uniaoR [] l = l
uniaoR (x:r) l = if pertence x l then uniaoR r l
                                 else x: uniaoR r l


--Ex 2
npares::[Int]->Int
npares [] = 0
npares (x:r) = if (mod x 2 == 0) then 1 + npares r
                                 else npares r

--Ex3
produtorio::[Int]->Int
produtorio [] = 1
produtorio (x:r) = x * produtorio r

--Ex4
comprime::[[Int]]->[Int]
comprime [] = []
comprime (x:r) = x ++ (comprime r)

--Ex5
tamanho::[j]->Int
tamanho [] = 0
tamanho (x:r) = 1 + tamanho r

--Ex 6
uniaoRec2::Eq t=>[t]->[t]->[t]
uniaoRec2 l [] = l
uniaoRec2 l (x:r) = if pertence x l then uniaoRec2 l r
                                    else uniaoRec2 (l++[x]) r