module Pain where
import System.IO
import Board


--GETS THE USER MOVE
getX = do
     line <- getLine
     let parsed = reads line :: [(Int, String)] in
       if length parsed == 0
       then getX'
       else let (x, _) = head parsed in
       if x > -2 && x < 7
         then return x
         else getX'
       where
       getX' = do
         putStrLn "Invalid input!"
         getX

gamebd = []

--TURNS A PLAYER INTO A CHAR
playerToChar p 
    | p == 1 = " X "
    | p == 2 = " O "
    | otherwise = " . "

--SWITCHES PLAYER TURN
switchPlayer p
    | p == 1 = 2
    | otherwise = 1

--CHECKS SLOT AND CONTINUES THE GAME
readSlot bd p = do
   putStrLn "Enter a slot you want to move"
   slot <- getX
   if slot  == -1 then putStrLn "Yeet yourself from the game."
   else do
     if isSlotOpen bd slot then do
        b <- return (dropInSlot bd slot p)
        gamebd <- return (boardToStr playerToChar b)
        putStrLn(gamebd)
        a <- return (switchPlayer p)
        d <- (gameOver b a)
        putStr("")

     else do 
       readSlot bd p

--playGameWithAI
readSlotWithAi bd p = do
   putStrLn "Enter a slot you want to move"
   slot <- getX
   if slot  == -1 then putStrLn "Yeet yourself from the game."
   else do
     if isSlotOpen bd slot then do
        b <- return (dropInSlot bd slot p)
        gamebd <- return (boardToStr playerToChar b)
        putStrLn(gamebd)
        a <- return (switchPlayer p)
        d <- (gameOverAI b a)
        putStr("")

     else do
       aiSlot bd 2
--	   	 
--Make Sure I do gameOverWith AI
aiSlot bd p = do
   putStrLn "AI Move"
   let slot = randomSlot bd 7
   if slot  == -1 then putStrLn "Yeet yourself from the game."
   else do
     if isSlotOpen bd slot then do
        b <- return (dropInSlot bd slot p)
        gamebd <- return (boardToStr playerToChar b)
        putStrLn(gamebd)
        a <- return (switchPlayer p)
        d <- (gameOverAI b a)
        putStr("")

     else do 
       readSlotWithAi bd 1

--Always passDefault as 6
--adding all the values and modding by 7 to perform a partial random
randomSlot bd defaultSlot
    | ((isSlotOpen board (fromIntegral(mod(sum(concat bd))7))) == True) = (fromIntegral(mod(sum(concat bd))7))
    | ((isSlotOpen board defaultSlot) == True) = defaultSlot
    | (defaultSlot == 0) = 0
    |  otherwise = randomSlot bd defaultSlot -1

--CHECKS TO SEE IF THE GAME HAS ENDED
----TODO: FIX VERTICAL WINS
gameOver bd p = do
     if (eachRow bd p 0 (length bd)) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (eachCol bd p 0 (length bd)) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (forwardDiagonals bd p 5 0) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (backDiagonals bd p 5 6) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if isFull bd then do
        putStrLn("The Game is a Draw!")
     else do
        readyPlayer bd p

gameOverAI bd p = do
     if (eachRow bd p 0 (length bd)) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (eachCol bd p 0 (length bd)) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (forwardDiagonals bd p 5 0) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if (backDiagonals bd p 5 6) then do
        putStrLn("Congratulations,"++playerToChar p++"! You won!")
     else if isFull bd then do
        putStrLn("The Game is a Draw!")
     else do
        currentPlayer bd p

--Checks Current Player in AI Game
currentPlayer bd p = do
    if (p == 1) then do
      readSlotWithAi bd 1
    else do
      aiSlot bd 2


--STARTS THE GAME
main = do
   putStrLn "Welcome To Connect 4!"
   mode <- getGameMode
   if mode  == 1 then do
     readSlotWithAi board 1
   else if mode == 69 then putStrLn"Nice"
   else do
     readyPlayer board 1

getGameMode = do
     putStrLn("Select '1' for single Player or '2' for Multiplayer!")
     line <- getLine
     let parsed = reads line :: [(Int, String)] in
       if length parsed == 0
       then getGameMode'
       else let (x, _) = head parsed in
       if x > -2
         then return x
         else getGameMode'
       where
       getGameMode' = do
         putStrLn "Invalid game mode, please try again!"
         getGameMode

--69 = secret game mode with colors in playerToChar

--ASKS THE PLAYER TO MAKE A MOVE
readyPlayer bd p = do
    putStrLn "Make a move!"
    readSlot bd p