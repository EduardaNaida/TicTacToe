import Data.Char

--jag älskar agnes

world = concat (replicate 3 row)
row =  "\n|" ++ slot 1 ++ "|" ++ slot 2 ++ "|" ++ slot 3 ++ "|\n"
slot n = currentBoard!!(n-1)

startBoard = replicate 9 "   "
currentBoard = startBoard

--Turn

data Turn = X | O
whichTurn = undefined
player = "1" --if whichTurn == X then "1" else "2"

--Move

playerInput :: IO ()

playerInput = do
    putStrLn $ "Player " ++ player ++ " move: "
    move <- getLine
    --currentBoard = insert whichTurn currentBoard
    putStrLn $ "Player " ++ player ++ " move: " ++ move
    


printWorld = dol 
    putStrLn world


main :: IO ()
main = do 
  putStrLn "\nTIC TAC TOE\n"
  --currentBoard <- startBoard
  play

play = do
    putStrLn $ "\nStatus: " ++ "\n" ++ world
    playerInput
    if win == True then putStrLn "GG" else play

win = False
