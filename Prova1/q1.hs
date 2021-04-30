figura::Int->Int->Int->String

figura a b c
   |a == 90 || b == 90 || c == 90 = "retangulo"
   |a > 90 || b > 90 || c > 90 = "obtuso"
   |a == b && b == c = "equilatero"
   |otherwise = "simples"
   
> figura 30 90 60