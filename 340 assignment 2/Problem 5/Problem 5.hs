takex n _      | n <= 0 =  []
takex _ []              =  []
takex n (x:xs)          =  x : takex (n-1) xs

dropx n xs     | n <= 0 =  xs
dropx _ []              =  []
dropx n (_:xs)          =  dropx (n-1) xs


spilt list1 n = (list3,list4)
  where list3 = takex n list1
        list4 = dropx n list1