module Board where
import Prelude

mkBoard m n = [[0|x<-[1..m]] | x<-[1..n]]
board = mkBoard 7 6
mkPlayer = 1
mkOpponent = 2

dropIntoSlot [] p = []
dropIntoSlot [_] p = [p]
dropIntoSlot (x:y:theRest) p
    | y == 0 = x : dropIntoSlot (y:theRest) p
    | otherwise =  p : y: theRest

displayBoard board  = mapM_ print board

replaceRow rowbd c p = [take c rowbd ++[p]++ drop (c+1) rowbd]

rebuildBoard bd r c p = take r bd ++ (replaceRow(bd!!r) c p) ++ drop (r+1) bd

dropInSlot bd col p = rebuildBoard bd (getRowIndex (getCol bd col) 0) col p

boardToStr playerToChar bd = rowToString playerToChar (concat bd) 1

rowToString playerToChar [] i = " 0  1  2  3  4  5  6"
rowToString playerToChar (h:t) i
    | ((mod i 7) == 0 ) = (playerToChar h)++"\n"++(rowToString playerToChar t 1)
    | otherwise = (playerToChar h)++(rowToString playerToChar t (i+1))

isSlotOpen bd i = (bd !! 0) !! i == 0
numSlot bd = length (bd !! 0)
isFull bd = hasNoZero (bd !! 0)

hasNoZero [] = True
hasNoZero (head:tail)
    | head == 0 = False
    | otherwise = hasNoZero tail

-- This code changes the element at (row,col) of a 2d array
changeElem [x] 0 col elem = changeRow x col elem
changeElem (h:t) 0 col elem = changeRow (t !! 0) col elem
changeElem (h:t) row col elem = changeElem t (row-1) col elem 


--getRowIndex takes a column gets the index of the first non zero -1
getRowIndex [] i = i-1
getRowIndex (h:t) i 
     | h /= 0  = i-1
     | otherwise = getRowIndex t (i+1)

changeRow [] i elem = []
changeRow (h:t) 0 elem = elem:t 
changeRow (h:t) i elem = h:changeRow t (i-1) elem

getRow bd index = bd !! index
getCol bd index = map (!! index) bd


--pass length of bd-1 as row param
eachRow bd p i 0 = checkWinRow (bd !! 0) p 0
eachRow bd p i row = (checkWinRow (bd !! i) p 0) || (eachRow bd p (i+1) (row-1))
--pass length of (bd !! 0)-1 as col param
eachCol bd p i 0 = checkWinRow (getCol bd i) p 0
eachCol bd p i col = (checkWinRow (getCol bd i) p 0) || (eachCol bd p (i+1) (col-1))


checkWinRow [] p count = False
checkWinRow (h:t) p count
    | (count == 4 || (count == 3 && h==p)) = True
    | h == p = checkWinRow t p (count+1)
    | otherwise = checkWinRow t p 0

-- check Between [0-3][3-5] for diagonal starts
-- If i can get each diagonal into a list I can pass into checkwinRow
--checkBetween
-- getDiagonal board, row, col, (0-3 for checking how many elements we have)

-- This code retrieves the element at (row,col) of a 2d array
getElem bd row col = (bd!!row) !! col

--if row >2 && col < 4
--eachDiagonal bd p row col |
--     | (row < 2)
-- returns a list of 4 elements in the diagonal from the given starting point
--     0 1 2 3 4 5 6
--0[[0,0,0,0,0,0,0]
--1	[0,0,0,0,0,0,0]
--2	[0,0,0,1,0,0,0]
--3	[0,0,1,0,0,0,0]
--4	[0,1,0,0,0,0,0]
--5	[1,0,0,0,0,0,0]]

--getDiagonal bd 5 0 returns [1,1,1,1]

getDiagonal [] _ _ _ = []
getDiagonal bd row col 3 = [fromIntegral (getElem bd row col)]
getDiagonal bd row col i = [fromIntegral (getElem bd row col)] ++ getDiagonal bd (row-1) (col+1) (i+1)

getBackDiagonal [] _ _ _ = []
getBackDiagonal bd row col 3 = [fromIntegral (getElem bd row col)]
getBackDiagonal bd row col i = [fromIntegral (getElem bd row col)] ++ getBackDiagonal bd (row-1) (col-1) (i+1)

--always start call with row = 5 col = 6
backDiagonals [] p row col = False;
backDiagonals bd p row col
     | (row>3 && col>2) = checkWinRow (getBackDiagonal bd row col 0) p 0 || backDiagonals bd p (row-1) col || backDiagonals bd p row (col-1)
     | otherwise = False


--always start call with row = 5 col = 0
forwardDiagonals [] p row col = False;
forwardDiagonals bd p row col
     | (row>3 && col <4) = checkWinRow (getDiagonal bd row col 0) p 0 || forwardDiagonals bd p (row-1) col || forwardDiagonals bd p row (col+1)
     | otherwise = False
