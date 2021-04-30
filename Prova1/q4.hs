sequencia::String->String->Bool
sequencia [] _ = True
sequencia n [] = False
sequencia lst (y:lst2) = if lst == (y:lst2) then True
                         else sequencia lst lst2

"la" "bola"