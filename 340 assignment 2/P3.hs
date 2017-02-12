data MyFloat = MyFloat (Integer,Integer)
  deriving Show
instance Eq MyFloat where
  (MyFloat (n,l)) == (MyFloat (m,p)) =(n Prelude.== m && l Prelude.== p)
  (MyFloat (n,l)) /= (MyFloat (m,p)) = (n Prelude./= m || l Prelude./= p)
instance Ord MyFloat where
  (MyFloat (n,l)) > (MyFloat (m,p)) = if (l Prelude.> p)
                                      then True
                                      else if (l Prelude.== p)
                                      then
                                        if (n Prelude.> m)
                                        then True
                                        else False
                                      else False
  (MyFloat (n,l)) < (MyFloat (m,p)) = if (l Prelude.< p)
                                      then True
                                      else if (l Prelude.== p)
                                      then
                                        if (n Prelude.< m)
                                        then True
                                        else False
                                      else False
  (MyFloat (n,l)) >= (MyFloat (m,p)) = ((MyFloat (n,l)) == (MyFloat (m,p))) || (MyFloat (n,l)) > (MyFloat (m,p))
  (MyFloat (n,l)) <= (MyFloat (m,p)) = ((MyFloat (n,l)) == (MyFloat (m,p))) || (MyFloat (n,l)) < (MyFloat (m,p))
instance Num MyFloat where
  (MyFloat (n,l)) + (MyFloat (m,p)) = if (l Prelude.== p)
                                      then (MyFloat (floor (reflow((flow n) Prelude.+ (flow m))),l))
                                      else
                                        if ((MyFloat (n,l)) > (MyFloat (m,p)))
                                        then (MyFloat (floor (reflow(((flow n) Prelude.* (10 Prelude.^ (l Prelude.- p))) Prelude.+ (flow m))),l))
                                        else (MyFloat (floor (reflow(((flow m) Prelude.* (10 Prelude.^ (p Prelude.- l))) Prelude.+ (flow n))),p))
  (MyFloat (n,l)) - (MyFloat (m,p)) =  if (l Prelude.== p)
                                       then (MyFloat (floor (reflow((flow n) Prelude.- (flow m))),l))
                                       else
                                         if ((MyFloat (n,l)) < (MyFloat (m,p)))
                                         then (MyFloat (floor (reflow(((flow n) Prelude.* (10 Prelude.^ (l Prelude.- p))) Prelude.- (flow m))),l))
                                         else (MyFloat (floor (reflow(((flow m) Prelude.* (10 Prelude.^ (p Prelude.- l))) Prelude.- (flow n))),p))
  (MyFloat (n,l)) * (MyFloat (m,p)) =  if(a>(10^(l+p))) 
                                       then MyFloat(n*m,(max l p)+1)
					                   else MyFloat(n*m,(max l p))
  (MyFloat (n,l)) / (MyFloat (m,p)) =  if(a>=(10^((max l p) - (min l p)))) 
                                       then MyFloat(n*m,((max l p) - (min l p)) + 1)
					                   else MyFloat(n*m,((max l p) - (min l p)))										 
  negate (MyFloat (n,l))            = MyFloat ((0 Prelude.- n),l) 

flowing a | a < 10    = 0
          | otherwise = flow (a/10)+1  
between a = if(a>0 && a<1) then True else False  
flow :: (Num a) => a -> Float
flow a = until (between) (/10) a
isInt x = x == fromInteger (round x)
reflow :: (Num a)=> a -> Integer
reflow a = until (isInt) (*10) a
value :: MyFloat -> Float
value (MyFloat (a,b)) = (flow a) Prelude.* 10 Prelude.^ b
whole :: MyFloat -> Integer
whole (MyFloat (a,b)) = floor (value (MyFloat (a,b)))
fraction :: MyFloat -> Float
fraction (MyFloat (a,b)) = (value (MyFloat (a,b))) Prelude.-  fromInteger (whole (MyFloat (a,b)))
converter :: MyFloat -> (Integer,Float)
converter (MyFloat (a,b)) = (x,y)
  where x = (value (MyFloat (a,b)))
        y = (value (MyFloat (a,b))) Prelude.- fromInteger (floor x)
                         