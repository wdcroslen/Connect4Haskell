import Prelude hiding(take)

main = print (fac 20)

fac 0 = 1
fac n = n * fac (n-1)


hamming 0 = False
hamming 1 = True
hamming n | mod n 5 == 0 = hamming (div n 5)
hamming n | mod n 3 == 0 = hamming (div n 3)
hamming n | mod n 2 == 0 = hamming (div n 2)
hamming n = False







--mult :: [Int] -> [Int]
--mult 5 [1,2,3] = [5,10,15]
--mult _ [] = []
mult n [] = []
mult n (h:t) = n*h : mult n t

--isSorted :: [Int] -> [Int]
isSorted [] = True
isSorted [_] = True

isSorted (x:y:theRest)
    | x<=y = isSorted(y:theRest)
    | otherwise  = False



--toCol [int]-> int - > [int]
--	column    player   result

makeMove [] p = []
makeMove [_] p = [p]
makeMove (x:y:theRest) p
    | y == 0 = x : makeMove (y:theRest) p
    | otherwise =  p : y: theRest

changeLast n [] = []
changeLast n [_] = [n]
changeLast n (x:y) = x : changeLast n y



--take int [int] - > [int]
take _ [] = []
take 0 _ = []
take n (h:t) = h : take (n-1) t

--isFull [int] -> Bool
isFull (x:y) 
    | x==0 = False
    | otherwise = True



--Similar to is sorted but remove head each call

--
--		row/col/diag    player   
--hasWinSequence: [int]-> int - > Bool