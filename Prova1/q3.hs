retira_varias::Eq t =>[t]->[t]->[t]
retira_varias [] n = n
retira_varias n [] = []
retira_varias (x:lst) lst2 = retira_varias lst (retira x lst2)

retira::Eq t =>t->[t]->[t]
retira n [] = []
retira n (y:lst) = if n == y then lst
                   else y:(retira n lst)