module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    import Data.List
    import Data.Foldable

    {- The type Point represents the gameplan. The first Int corresponds to the vertical row,
       and the second Int is the horizontal row
     -}
    type Point = (Int, Int)

     {- The type Board takes a list of lists of the data type Slot 
     -}
    type Board = [[Slot]]
    
    {-The Slot data type represents an empty or full player in a slot
   -}
    data Slot = Empty | Full Player deriving(Eq)

    {-The Player data type represents the player, being either X or O
   -}
    data Player = X | O deriving(Eq)

    instance Show Slot where
        show Empty = " "
        show (Full X) = "X"
        show (Full O) = "O"
    
    instance Show Player where
        show X = "X"
        show O = "O"

    {-horizontalRow row
        Prints a given row of a Board
        RETURNS: A row representing the slots as a string
        EXAMPLES: horizontalRow [Empty, Full X, Empty] = "  | X |  "
                  horizontalRow [Full O, Empty, Full X] = "O |   | X"
    -}
    horizontalRow :: [Slot] -> String
    horizontalRow row = intercalate " | " $ fmap show row

    {-  dividingLine string
        Prints a string representing a divider (-) between horizontal rows
        RETURNS: A string with the sign - . Which length depends on how long the inputted string is
        EXAMPLES: dividingLine "  | X |  "  = "---------"
                  dividingLine " " = "-"

    -}
    dividingLine :: String -> String
    dividingLine string = concat $ replicate (length string) "-"

    {- printBoard board
        Prints the board, containing the horizontal rows combined with the dividing lines
        SIDE EFFECTS: Prints the board
        EXAMPLES: printBoard [[Empty,Full X,Empty],[Full O,Empty,Full O]] =    | X |
                                                                             ---------
                                                                             O |   | O
                                                                             ---------
    -}
    printBoard :: Board -> IO ()
    printBoard board = for_ board $ \row -> do
        putStrLn $ horizontalRow row
        putStrLn $ dividingLine (horizontalRow row)

    {-startingPlayer
        Randomly decides a player 
        RETURNS: X or O at random
        SIDE EFFECTS: Updates the RNG seed
        EXAMPLES: startingPlayer = O
                  startingPlayer = X
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
        RETURNS: The stored Int
        SIDE EFFECTS: Updates the RNG seed
        EXAMPLES: startingPlayerAux = 1
                  startingPlayerAux = 2
    -}
    startingPlayerAux :: IO Int
    startingPlayerAux = do 
        a <- randomRIO (1,2)
        return a

    {-nextPlayer Player
        Switches turnes for the players
        RETURNS: X or O
    -}

    nextPlayer :: Player -> Player
    nextPlayer X = O
    nextPlayer O = X
    
    {-playerInsert Player
        Corresponds the Player to a Slot
        RETURNS: Full X or Full O
    -}

    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    {-initialBoard (m,n)
        Creates the initial gameplan
        RETURNS: An empty board, where the first int is how many lists and the second int is how many element
        should be in those lists
        EXAMPLES: initialBoard (3,3) == [[ , , ],[ , , ],[ , , ]]
                  initialBoard (3,1) == [[ ],[ ],[ ]]
                  initialBoard (1,0) == [[]]
                  initialBoard (0,1) == []
    -}

    initialBoard :: (Int,Int) -> Board
    initialBoard (m,n) = replicate m (replicate n Empty)

    {- makeMove point board player
        Updates the Board from an input Point
        RETURNS: Board where Player has been inserted into the Slot of the Point
        EXAMPLES: makeMove (1,2) [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X == [[ , , ],[ , ,X],[ , , ]]
                  makeMove (0,0) [[Empty],[Empty],[Empty]] O == [[O],[ ],[ ]]
    -}

    makeMove :: Point -> Board -> Player -> Board
    makeMove point board player = replaceBoard board point (Full player)
    
    {- replaceBoard board point slot
        Takes the current board and returns the new board with an added full slot 
        RETURNS: Board where a point is updated
        EXAMPLES: replaceBoard [[Full X,Full X,Empty],[Full O,Empty,Empty],[Full O,Empty,Empty]] (0,2) (Full X) == [[X,X,X],[O, , ],[O, , ]]
                  replaceBoard [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) (Full O) == [[[O, , ],[ , , ],[ , , ]]
    -}
    
    replaceBoard :: Board -> Point -> Slot -> Board
    replaceBoard board point slot = replaceList board (fst point) (replaceList (board !! (fst point)) (snd point) slot)

     {- replaceList list int insert
        Creates a new list from the old by replacing an element 
        RETURNS: List containing the original list with one replaced element
        EXAMPLES: replaceList [Empty,Full O,Empty] 2 (Full X) = [ ,O,X]
                  replaceList [Empty,Empty,Empty] 0 (Full O) = [O, , ]
    -}

    replaceList :: [a] -> Int -> a -> [a]
    replaceList list int insert = x ++ insert : ys
        where (x,_:ys) = splitAt int list

    {- readMove
        Checks if the input is a valid point, and if so reduces the numbers by one int the point (x-1,y-1)
        RETURNS: The reduced input Point 
        SIDE EFFECTS: Reads one or more lines from standard input 
    -}

    readMove :: IO Point
    readMove = do
        str <- getLine
        case readMaybe str of
            Just point -> return (((fst point) - 1), ((snd point) - 1))
            Nothing    -> do
                putStrLn "Invalid input. Try the format (x,y) \n Where x is the vertical row number and y is the horizontal index"
                readMove

    {- pointValid board point
        Checks if a point is not negative, within the board and not empty
        RETURNS: True if the point follows the conditions staded for it to be valid and false otherwise
        EXAMPLES: pointValid [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) == True
                  pointValid [[Full X,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) == False
                  pointValid [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (100,0) == False
    -}

    pointValid :: Board -> Point -> Bool
    pointValid board point = (fst point) >= 0 && (fst point) < (length board) && (snd point) >= 0 && (snd point) < (length (board !! 0)) && isEmpty board point

     {- isEmpty board point
        Checks if a point on the board is empty or not
        RETURNS: True if the Slot on the board is empty and false if the Slot is Full
        EXAMPLES: isEmpty [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty] (0,0) = True
                 isEmpty [[Full O,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) = False
    -}

    isEmpty :: Board -> Point -> Bool
    isEmpty board point = ((board !! (fst point)) !! (snd point)) == Empty

   {-tieCount int
        Removes 1 from an Int
        RETURNS: Int - 1
        EXAMPLES: tieCount 5 == 4
   -}

    tieCount :: Int -> IO Int
    tieCount int = do
        return (int - 1)

    
    {- readInt
        Reads an input and prints it out if 
        RETURNS: 
        SIDE EFFECTS:
    -}

    readInt :: IO Int
    readInt = do
        str <- getLine
        case readMaybe str of
            Just int -> if int > 1 then return int else
                do
                    putStrLn "Value must be larger than 1"
                    readInt
            Nothing    -> do
                putStrLn "Invalid input. Enter one number at a time"
                readInt


    {- runGame
        Runs the game
        Side-effect: The game interaction
    -}
    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        putStrLn $ "How many rows do you want?"
        m <- readInt
        putStrLn $ "How many columns do you want?"
        n <- readInt
        putStrLn $ "How many slots in a row should be required to win?"
        k <- readInt
        let count = m * n
        gameLoop count player (initialBoard (m,n)) (m,n,k)
    
    {- gameLoop count player board
       Plays the game
       Side effect: Reads one or more lines from standard input and prints strings
    -}
    gameLoop :: Int -> Player -> Board -> (Int,Int,Int) -> IO ()
    gameLoop count player board (m,n,k) = do
      printBoard board
      newCount <- tieCount count
      if newCount >= 0 then do
            putStrLn $ "What will " ++ show player ++ " do?"
            point <- readMove
            let valid = pointValid board point
            if valid then do
                let newBoard = makeMove point board player
                let newPlayer = nextPlayer player
                if (checkWin player newBoard (m,n,k)) then do
                    printBoard newBoard
                    putStrLn $ "Player " ++ show player ++ " wins!!"
                else do
                    putStrLn "Smart move! :)"
                    gameLoop newCount newPlayer newBoard (m,n,k)
            else do
                putStrLn "Your point is out of bounds or occupied! Try again!"
                gameLoop count player board (m,n,k)
        else do
            putStrLn "It's a tie!"
    
    {-checkWin player board (m,n,k)

    -}

    checkWin :: Player -> Board -> (Int,Int,Int) -> Bool
    checkWin player board (m,n,k) = any (all (== (Full player))) $ winCases board (m,n,k)

    {-diagonal board
    
    -}

    diagonal :: [[a]] -> [a]
    diagonal []           = []
    diagonal ((x:_):rows) = x : diagonal (map tail rows)

    {-diags board
    
    -}

    diags :: [[a]] -> [[a]]
    diags board = map diagonal (init . tails $ board) ++ tail (map diagonal (init . tails $ transpose board))

    {-winCases board (m,n,k)
    
    -}

    winCases :: Board -> (Int,Int,Int) -> [[Slot]]
    winCases board (m,n,k) =
        filter ((== k) . length) $ rows ++ cols ++ allDiags
        where
            rows = concatMap inRow $ [board !! i | i <- [0..(m-1)]]
            cols = concatMap inRow $ transpose board
            allDiags = concatMap inRow $ diags board ++ diags (map reverse board)

    {-inRow diag
    
    -}

    inRow :: [Slot] -> [[Slot]]
    inRow diag = concatMap tails (map reverse(tails diag))
