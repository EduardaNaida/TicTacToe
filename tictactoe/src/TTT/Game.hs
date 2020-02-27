module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    import Data.List
    import Data.Foldable
    
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
    printBoard board = for_ board $ \row -> do
        putStrLn $ verticleRow row
        putStrLn horizontalRow

        -- for_ :: [a] -> (a -> IO b) -> IO ()
        -- for :: [a] -> (a -> IO b) -> IO [b]

    
    --Turn
    
    --data Turn = PlayerX | PlayerO deriving(Show, Eq)
    -- ^^ behövs inte enligt Love

    {-startingPlayer
    Randomly decides which player should start the game.
    Returns: X or O at random
    Side-Effects: Updates the RNG seed
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

    nextPlayer :: Player -> Player
    nextPlayer X = O
    nextPlayer O = X
    
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
    --b: pos. in row
    --numbers a and b range from 0 to 2
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
            Just point -> return (((fst point) - 1), ((snd point) - 1))
            Nothing    -> do
                putStrLn "Invalid input. Try the format (x,y) \n Where x is the vertical row number and y is the horizontal index"
                readMove

    pointValid :: Board -> Point -> Bool
    pointValid board point = (fst point) >= 0 && (fst point) < 3 && (snd point) >= 0 && (snd point) < 3 && isEmpty board point

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
      if newCount >= 0 then do
            putStrLn $ "What will " ++ show player ++ " do?"
            point <- readMove
            let valid = pointValid board point
            if valid then do
                newBoard <- makeMove point board player
            --win <- checkWin point newBoard
            -- använd makeMove för nytt bräde. Om invalid, kör samma gameLoop igen
            -- om valit, kolla vinst; om ingen vinst, kör gameLoop på det nya brädet, dekrementera movesLeft, och byt spelare
                let newPlayer = nextPlayer player
                putStrLn "Smart move! :)"
                gameLoop newCount newPlayer newBoard
            else do
                putStrLn "Your point is out of bounds or occupied! Try again!"
                gameLoop count player board
        else do
            putStrLn "It's a tie!"
    
    
    
    checkWin :: Point -> Board -> Maybe Player
    checkWin = undefined
    
    -- Behöver kolla draws. Ett sätt är att bära omkring en räknare som dekrementeras med varje legalt drag; börjar på antalet spaces; oavgjort när den når 0 utan att nån har vunnit
