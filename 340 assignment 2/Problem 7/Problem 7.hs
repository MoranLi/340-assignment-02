encode []     = []
encode (x:xs) = encode' 1 x xs 
  where encode' n x [] = [(n)]
        encode' n x (y:ys) | x == y    = encode' (n + 1) x ys
                           | otherwise = (n) : encode' 1 y ys

maximum2 n [] = n
maximum2 n (x:xs) = if (x>n) 
                    then maximum2 x xs
                    else maximum2 n xs					
						   
consectived []    = 0
consectived list1 = k
  where k = maximum2 0 (encode list1)
            
             