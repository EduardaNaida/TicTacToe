module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    
    data Slot = Empty | Full Player
    data Player = X | O deriving(Show, Eq)
    
    instance Show Slot where
        show Empty = " "
        show (Full X) = "X"
        show (Full O) = "O"
    
    instance Show Player where
        show X = "X"
        show O = "O"
    
    world = "\n" ++ show (slot 1) ++ "|"
    
    slot n = currentBoard!!(n-1)
    
    currentBoard = replicate 9 Empty
    
    --Turn
    
    --data Turn = PlayerX | PlayerO deriving(Show, Eq)
    -- ^^ behövs inte enligt Love
    
    startingPlayer :: IO Player
    startingPlayer = do
        i <- yesno
        if (i == 1) then
            return X
        else
            return X
    
    
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    initialBoard :: Board
    
    printBoard :: Board -> IO ()
    
    -- För att parsea moves, använd readMaybe från Text.Read
    
    readMove :: IO Point
    readMove = do
        str <- getLine
        case readMaybe str of
            Just point -> return point
            Nothing    -> do
                putStrLn "Invalid input."
                readMove
    
    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        gameLoop player board
    
    gameLoop :: Int -> Player -> Board -> IO ()
    gameLoop movesLeft turn board = do
      printBoard board
      -- kolla om oavgjort
      putStrLn $ "What will " ++ show turn ++ " do?"
      point <- readMove
      -- använd makeMove för nytt bräde. Om invalid, kör samma gameLoop igen
      -- om valit, kolla vinst; om ingen vinst, kör gameLoop på det nya brädet, dekrementera movesLeft, och byt spelare
    
    --ger ut antingen 1 eller 2
    yesno :: IO Int
    yesno = do 
        a <- randomRIO (1,2)
        return a
    
    
    type Point = (Int, Int)
    
    newtype Board = Board [[Slot]]
    
    makeMove :: Point -> Board -> Player -> Maybe Board
    
    checkWin :: Point -> Board -> Maybe Player
    
    -- Behöver kolla draws. Ett sätt är att bära omkring en räknare som dekrementeras med varje legalt drag; börjar på antalet spaces; oavgjort när den når 0 utan att nån har vunnit
    
    
    --använd för att uppdatera en lista med ett nytt element 'insert'
    replaceList :: [a] -> Int -> a -> [a]
    replaceList list int insert = x ++ insert : ys
        where (x,_:ys) = splitAt int list