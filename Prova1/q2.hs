seleciona::[[Int]]->[Int]
seleciona lst = [head(y) | y <- lst, length y > 1, head(tail y) < 5]
