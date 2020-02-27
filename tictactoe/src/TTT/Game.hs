module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    import Data.List
    import Data.Foldable
    
    {- The type Point represent the gameplan, where the first Int is the vertical row where 0 is the row highest up,
     the second Int represent the horizontal row.
     Invariant: 0 - 2
     -}
    type Point = (Int, Int)

     {- The type Board takes a list of lists of the data type Slot 
     -}
    type Board = [[Slot]]
    
    {-The Slot data type represents an empty or full player in a slot
   -}
    data Slot = Empty | Full Player deriving(Eq)

    {-The Player data type represent the player who can be either X or O
   -}
    data Player = X | O deriving(Eq)

    instance Show Slot where
        show Empty = " "
        show (Full X) = "X"
        show (Full O) = "O"
    
    instance Show Player where
        show X = "X"
        show O = "O"

    {-verticleRow row
    Creates the verticle row
    Returns: String representing a row
    -}
    verticleRow :: [Slot] -> String
    verticleRow row = intercalate " | " $ fmap show row

    {-horizontalRow
    Prints a string representing a row
    -}
    horizontalRow :: String
    horizontalRow = "----------"

    {- printBoard board
       Side effects: Prints the board
    -}
    printBoard :: Board -> IO ()
    printBoard board = for_ board $ \row -> do
        putStrLn $ verticleRow row
        putStrLn horizontalRow

        -- for_ :: [a] -> (a -> IO b) -> IO ()
        -- for :: [a] -> (a -> IO b) -> IO [b]

    {-startingPlayer
    Randomly decides a game player.
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
    
    {-startingPlayerAux
    Randomly decides between 1 or 2 and stores it in an Int
    Returns: The stored Int
    Side-effect: Updates the RNG seed
    -}
    startingPlayerAux :: IO Int
    startingPlayerAux = do 
        a <- randomRIO (1,2)
        return a

    {-nextPlayer Player
    Switches turnes for the players
    Returns: X or O
    -}
    nextPlayer :: Player -> Player
    nextPlayer X = O
    nextPlayer O = X
    
    {-playerInsert Player
    Corresponds the Player to a Slot
    Returns: Full X or Full O
    -}
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    {-initialBoard
    Creates the initial gameplan
    Returns: An empty board
    Example: InitialBoard = [[ , , ],[ , , ],[ , , ]]
    -}
    initialBoard :: Board
    initialBoard = replicate 3 (replicate 3 Empty)

    {-
    If we want to generate a board with and arbitrary number of slots:

    genBoard :: Int -> Board
    genBoard int = Board replicate int (replicate int Empty)
    -}

    {- makeMove point board player
    Creates a Board from input Point
    Returns: Board where Player has been inserted into the Point
    Example: makeMove (1,2) [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X = [[ , , ],[ , ,X],[ , , ]]
    -}
    makeMove :: Point -> Board -> Player -> IO Board
    makeMove point board player = do
        return $ replaceBoard board point (Full player)
    
    {- replaceBoard board point slot
        Takes the current board and prints the new board with an added full slot 
        Returns: Board where the Player's move is updated
        Examples: replaceBoard [[Full X,Full X,Empty],[Full O,Empty,Empty],[Full O,Empty,Empty]] (0,2) (Full X) = [[X,X,X],[O, , ],[O, , ]]
                  replaceBoard [[Full X,Empty,Empty],[Full O,Empty,Full O],[Full X,Full X,Empty]] (1,1) (Full O) = [[X, , ],[O,O,O],[X,X, ]]
    -}
    replaceBoard :: Board -> Point -> Slot -> Board
    replaceBoard board point slot = replaceList board (fst point) (replaceList (board !! (fst point)) (snd point) slot)

     {- replaceList list int insert
        Creates a new list from the old by replacing an element 
        Returns: List containing the original list with one replaced element
        Examples: replaceList [Empty,Full O,Empty] 2 (Full X) = [ ,O,X]
                  replaceList [Empty,Full O,Empty] 0 (Full O) = [O,O, ]
    -}
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

     {- isEmpty board point
        Checks if a Slot is empty or not
        Returns: True if the Slot is empty and false if the Slot is Full
        Example: isEmpty [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Full X]] (0,0) = True
                 isEmpty [[Full O,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Full X]] (0,0) = False
    -}
    isEmpty :: Board -> Point -> Bool
    isEmpty board point = ((board !! (fst point)) !! (snd point)) == Empty
    
   {-tieCount int
     Removes 1 from an Int
     Returns: input Int - 1
   -}
    tieCount :: Int -> IO Int
    tieCount int = do
        return (int - 1)

    {-main
      Calls runGame
    -}
    main :: IO ()
    main = do
        runGame

    {- runGame
        Runs the game
        Side-effect: The game interaction
    -}
    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        gameLoop 9 player initialBoard
    
    {- gameLoop count player board
       Plays the game
       Side effect: Reads one or more lines from standard input and prints strings
    -}
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
    
    
    --checkWin :: Point -> Board -> IO (Maybe Player)
    --checkWin point board = do



    -- Behöver kolla draws. Ett sätt är att bära omkring en räknare som dekrementeras med varje legalt drag; börjar på antalet spaces; oavgjort när den når 0 utan att nån har vunnit
