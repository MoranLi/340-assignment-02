takex n _      | n <= 0 =  []
takex _ []              =  []
takex n (x:xs)          =  x : takex (n-1) xs

dropx n xs     | n <= 0 =  xs
dropx _ []              =  []
dropx n (_:xs)          =  dropx (n-1) xs

spiltHalf list1 = (list3,list4)
  where list3 = takex ((length list1) `div` 2) list1
        list4 = dropx ((length list1) `div` 2) list1
        
shuffle (list1,[])            = list1
shuffle ([],list2)            = list2
shuffle ((a:list1),(b:list2)) = a : b : shuffle (list1,list2)
        
generater s _ | s <= 0 = []
generater s a          = a : generater (s-1) a


mixwhere (listk,listm) n
  | n == 0    = (listk ++ listm)
  | otherwise = mixwhere (spiltHalf (shuffle (listk,listm))) (n-1)  
        
nshuffle n c = mixwhere (list12,list23) n
  where list12 = generater c 'b'
        list23 = generater c 'r'
          
          
      