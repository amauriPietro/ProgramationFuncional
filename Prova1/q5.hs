representa::[String]->String->[String]
representa lst s = [y | y <- lst, verifica y s == True]

verifica::String->String->Bool
verifica plv s = [y | y <- plv, y /= 'a', y /= 'e' , y /= 'i', y /= 'o', y /= 'u'] == s