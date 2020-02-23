module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    
    type Point = (Int, Int)
    
    newtype Board = Board [[Slot]]

    data Slot = Empty | Full Player
    data Player = X | O deriving(Eq)
    
    instance Show Slot where
        show Empty = " "
        show (Full X) = "X"
        show (Full O) = "O"
    
    instance Show Player where
        show X = "X"
        show O = "O"
    
    
    --Turn
    
    --data Turn = PlayerX | PlayerO deriving(Show, Eq)
    -- ^^ behövs inte enligt Love
    
    startingPlayer :: IO Player
    startingPlayer = do
        i <- yesno
        if (i == 1) then
            return X
        else
            return O
    
    
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    initialBoard :: Board
    initialBoard = Board replicate 3 (replicate 3 Empty)

    {-
    genBoard :: Int -> Board
    genBoard int = Board replicate int (replicate int Empty)
    -}

    boardElem :: Board -> Point -> Slot
    boardElem board point = undefined

    printBoard :: Board -> IO ()
    printBoard = undefined

    --coordinates (a,b) 
    --a: horizontal row 
    --b: pos. in row (1-3)
    replaceBoard :: Board -> Point -> a -> Board
    replaceBoard (Board list) point player = Board replaceList (list !! (fst point)) (snd point) player

    --använd för att uppdatera en lista med ett nytt element 'insert'
    replaceList :: [a] -> Int -> a -> [a]
    replaceList list int insert = x ++ insert : ys
        where (x,_:ys) = splitAt int list
    
    -- För att parsea moves, använd readMaybe från Text.Read
    
    {- readMove
        Checks if the standard input is a point 
        Returns: The input Point
        Side effect: Reads one or more lines from standard input 
    -}

    readMove :: IO Point
    readMove = do
        str <- getLine
        case readMaybe str of
            Just point -> return point
            Nothing    -> do
                putStrLn "Invalid input."
                readMove
    
    {- runGame
        Starts the game with either X or O's turn
        Returns: 
    -}

    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        gameLoop movesLeft player initialBoard
    
    gameLoop :: Int -> Player -> Board -> IO ()
    gameLoop movesLeft turn board = do
      printBoard board
      -- kolla om oavgjort
      putStrLn $ "What will " ++ show turn ++ " do?"
      point <- readMove
      -- använd makeMove för nytt bräde. Om invalid, kör samma gameLoop igen
      -- om valit, kolla vinst; om ingen vinst, kör gameLoop på det nya brädet, dekrementera movesLeft, och byt spelare
      putStrLn "tja"
    

    --ger ut antingen 1 eller 2
    yesno :: IO Int
    yesno = do 
        a <- randomRIO (1,2)
        return a
    
    
    makeMove :: Point -> Board -> Player -> Maybe Board
    makeMove point board player = undefined
    
    checkWin :: Point -> Board -> Maybe Player
    checkWin = undefined
    
    -- Behöver kolla draws. Ett sätt är att bära omkring en räknare som dekrementeras med varje legalt drag; börjar på antalet spaces; oavgjort när den når 0 utan att nån har vunnit
