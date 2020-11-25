import Prelude hiding(map,filter,foldl,foldl1)
--import Prelude hiding(map)

--map:: (a -> b) -> [a] -> [b]         Ex: map double [1,2,3,4,5]  --> [2,4,6,8,10]
map f [] = []
map f (h:t) = f h : map f t

--				[1,2,3,4]
--				h = 1
--				t = 2,3,4
--				
--				return [2]  +   [map double [2,3,4]]  
--				return [4]  +   [map double [3,4]]  
--				return [6]  +   [map double [4]]  
--				return [8]  +   [map double []] 
--				[2] + [4] + [6] + [8] + []
--				
--				[2,4,6,8]
--				
				
				
isTwo n = 2
isThree n = 3


--filter:: (a -> Bool) -> [a] -> [a]   Ex: filter odd [1,2,3,4,5]  --> [1,3,5]
filter f [] = []
filter f (h:t) 
    |  f h == True = h : filter f t
    |  otherwise = filter f t
	
--	(| = if )
--	(= return)  

--foldl:: (a -> b -> a) -> a -> [b] -> a    Ex: foldl max 5 [1,2,3,4,5]  --> 5
foldl1 f n [] = 0
foldl1 f n [e] = f n e
foldl1 f n (x:y) = foldl1 f (f n x) y


--foldl1:: (a -> a -> a) -> [a] -> a    Ex: foldl1 sum2 [1,2,3,4,5]  --> 15
foldl f [] = 0
foldl  f [e] = e
foldl f [x,y] = f x y 
foldl f (x:y) = f x (foldl f y)



--[1,2,3,4,5]
--sum2 1 2    = 3 
--sum2 (sum2 1 2) 3 
--sum2 6 4 
---sum2 10 5


--[1,2,3,4,5]

--max 1 2    = 2
--max (max 1 2) 3 
--max 3 4 
--max 4 5



sum2 n m = n + m


sub n m = n - m
mult n m = n * m


evenNum 0 = False
evenNum n 
    | ((mod n 2) == 1) = False
    | otherwise = True

oddNum n
    | ((mod n 2) == 1) = True
    | otherwise = False

double n = n + n