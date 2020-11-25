import Prelude hiding(take)

take _ [] = []
take 0 _ = []
take n (h:t) = h : take (n-1) t

makeMove [] p = []
makeMove [_] p = [p]
makeMove (x:y:theRest) p
    | (p /= 2 && p/=1) = []
    | y == 0 = x : makeMove (y:theRest) p
    | otherwise =  p : y: theRest