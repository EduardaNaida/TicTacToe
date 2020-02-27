module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    import Data.List
    
    type Point = (Int, Int)
    
    type Board = [[Slot]]

    data Slot = Empty | Full Player deriving(Eq)
    data Player = X | O deriving(Eq)

    instance Show Slot where
        show Empty = " "
        show (Full X) = "X"
        show (Full O) = "O"
    
    instance Show Player where
        show X = "X"
        show O = "O"

    verticleRow :: [Slot] -> String
    verticleRow row = intercalate " | " $ fmap show row

    horizontalRow :: String
    horizontalRow = "----------"

    {- printBoard board
        Prints the board in form of three lists containing three slots in each list
        Side effect: Prints the board
    -}
    printBoard :: Board -> IO ()
    printBoard board = do
        putStrLn $ verticleRow firstRow
        putStrLn horizontalRow
        putStrLn $ verticleRow secondRow
        putStrLn horizontalRow
        putStrLn $ verticleRow thirdRow
        where firstRow  = board !! 0
              secondRow =  board !! 1
              thirdRow  = board !! 2
    

    {-startingPlayer
    Randomly decides which player should start the game.
    Returns: IO X or O
    -}
    startingPlayer :: IO Player
    startingPlayer = do
        i <- startingPlayerAux
        if (i == 1) then
            return X
        else
            return O
    
    --ger ut antingen 1 eller 2
    startingPlayerAux :: IO Int
    startingPlayerAux = do 
        a <- randomRIO (1,2)
        return a

    nextPlayer :: Player -> IO Player
    nextPlayer X = do return O
    nextPlayer O = do return X
    
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    initialBoard :: Board
    initialBoard = replicate 3 (replicate 3 Empty)

    {-
    If we want to generate a board with and arbitrary number of slots:

    genBoard :: Int -> Board
    genBoard int = Board replicate int (replicate int Empty)
    -}

    


    makeMove :: Point -> Board -> Player -> IO Board
    makeMove point board player = do
        return (replaceBoard board point (Full player))


    --coordinates (a,b) 
    --a: horizontal row 
    --b: pos. in row (1-3)
    replaceBoard :: Board -> Point -> Slot -> Board
    replaceBoard board point slot = replaceList board (fst point) (replaceList (board !! (fst point)) (snd point) slot)

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

    pointValid :: Board -> Point ->  Bool
    pointValid board point = if (fst point) > 0 && (fst point) < 4 && (snd point) > 0 && (snd point) < 4 && isEmpty board point then True else False

    isEmpty :: Board -> Point -> Bool
    isEmpty board point = if ((board !! (fst point)) !! (snd point)) == Empty then True else False
    
    {- runGame
        Starts the game with either X or O's turn
        Returns: 
    -}
    tieCount :: Int -> IO Int
    tieCount int = do
        return (int - 1)

    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        gameLoop 9 player initialBoard
    
    gameLoop :: Int -> Player -> Board -> IO ()
    gameLoop count player board = do
      printBoard board
      newCount <- tieCount count
      -- kolla om oavgjort
      putStrLn $ "What will " ++ show player ++ " do?"
      point <- readMove
      --point <- pointValid
      newBoard <- makeMove point board player
      --win <- checkWin point newBoard
      -- använd makeMove för nytt bräde. Om invalid, kör samma gameLoop igen
      -- om valit, kolla vinst; om ingen vinst, kör gameLoop på det nya brädet, dekrementera movesLeft, och byt spelare
      newPlayer <- nextPlayer player
      putStrLn "Smart move! :)"
      gameLoop newCount newPlayer newBoard
    
    
    
    checkWin :: Point -> Board -> Maybe Player
    checkWin = undefined
    
    -- Behöver kolla draws. Ett sätt är att bära omkring en räknare som dekrementeras med varje legalt drag; börjar på antalet spaces; oavgjort när den når 0 utan att nån har vunnit
