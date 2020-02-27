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
     {- The type Board takes a lists of lists of the data type slots in the game
     -}
    type Board = [[Slot]]
    
    {-The Slot data type represents an empty or full player in a slot in the game. Is Comparable
     THe slot in the game can either be empty or full
   -}
    data Slot = Empty | Full Player deriving(Eq)

    {-The Player data type represent the player who can be an X or O. It can be compared.
     The player can either be X or O.
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
    Returns: String of slots as defined in the instance for Slot
    -}
    verticleRow :: [Slot] -> String
    verticleRow row = intercalate " | " $ fmap show row

    horizontalRow :: String
    horizontalRow = "----------"

    {- printBoard board
        Prints the board
    -}
    printBoard :: Board -> IO ()
    printBoard board = for_ board $ \row -> do
        putStrLn $ verticleRow row
        putStrLn horizontalRow

        -- for_ :: [a] -> (a -> IO b) -> IO ()
        -- for :: [a] -> (a -> IO b) -> IO [b]

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
    
    {-startingPlayerAux
    Randomly decides between 1 or 2 and stores it in a Int (a)
    Returns: IO a (Int)
    Side-effect: stores a value in a Int
    -}
    startingPlayerAux :: IO Int
    startingPlayerAux = do 
        a <- randomRIO (1,2)
        return a

    {-nextPlayer Player
    Switches turn for the players
    Returns: X or O
    -}
    nextPlayer :: Player -> Player
    nextPlayer X = O
    nextPlayer O = X
    
    {-playerInsert Player
    Takes a Players move and returns it as a slot in the game
    Returns: Full x or Full O
    -}
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    {-initialBoard
    Creates the base gameplan
    Returns: a board represented by list of lists
    Ex: InitialBoard == [[ , , ],[ , , ],[ , , ]]
    -}
    initialBoard :: Board
    initialBoard = replicate 3 (replicate 3 Empty)

    {-
    If we want to generate a board with and arbitrary number of slots:

    genBoard :: Int -> Board
    genBoard int = Board replicate int (replicate int Empty)
    -}

    {- makeMove point board player
        Updates the board with an new one updated with the player who made a move
    Returns: IO Board
    -}
    makeMove :: Point -> Board -> Player -> IO Board
    makeMove point board player = do
        return $ replaceBoard board point (Full player)


    --coordinates (a,b) 
    --a: horizontal row 
    --b: pos. in row
    --numbers a and b range from 0 to 2
    
    {- replaceBoard board point slot
        Takes the board and changes the point in the gameplan with an updated one.
        Returns: The gamplan where the players moves are updated
        Example: replaceBoard [[Full X],[Full O],[Full X]] (0,0) Empty == [[ ],[O],[X]]
        replaceBoard [[Empty],[Full O],[Full X]] (0,0) (Full O)  == [[O],[O],[X]]
    -}
    replaceBoard :: Board -> Point -> Slot -> Board
    replaceBoard board point slot = replaceList board (fst point) (replaceList (board !! (fst point)) (snd point) slot)

     {- replaceList list int insert
         Used for updating a list with a new element: insert. Takes the insert and switch the place with the element in the list after
        the Int taken as an argument.
        Returns: A list with the insert in it
        Example: replaceList [1,2,3] 2 4 == [1,2,4]
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
        Checks if a point in the board is empty or not
        Returns: True if the point is empty and false if it's not empty
        Example: isEmpty [[Full X],[Full O],[Full X]] (0,0) == False
    -}

    isEmpty :: Board -> Point -> Bool
    isEmpty board point = if ((board !! (fst point)) !! (snd point)) == Empty then True else False
    
    {- runGame
        Starts the game with either X or O's turn
        Returns: 
    -}
    tieCount :: Int -> IO Int
    tieCount int = do
        return (int - 1)


    main :: IO ()
    main = do
        runGame

    {- runGame
        Runs the game
        Side-effect: The game interaction 
                Reading input from the keyboard
                Printing output on the screen
    -}

    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        gameLoop 9 player initialBoard
    
    {- gameLoop count player board
         
         Pre: 
         Returns:
         Side-effect:
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
