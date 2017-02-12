shuffle list1 [] = list1
shuffle [] list2 = list2
shuffle (a:list1) (b:list2) = a : b : shuffle list1 list2

