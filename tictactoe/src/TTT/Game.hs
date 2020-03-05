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
        RETURNS: A string with the sign - . Which length depends on how long the input string is
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
        Picks a random player
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
        Generates a random Int that is either 1 or 2
        RETURNS: 1 or 2 at random
        SIDE EFFECTS: Updates the RNG seed
        EXAMPLES: startingPlayerAux = 1
                  startingPlayerAux = 2
    -}
    startingPlayerAux :: IO Int
    startingPlayerAux = do 
        a <- randomRIO (1,2)
        return a

    {-nextPlayer player
        Switches turns for the players
        RETURNS: X or O
    -}
    nextPlayer :: Player -> Player
    nextPlayer X = O
    nextPlayer O = X
    
    {-playerInsert player
        Returns the Slot that corresponds to a certain player
        RETURNS: Full X or Full O
    -}
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    {-initialBoard (m,n)
        Generates the initial game board
        RETURNS: An empty board with height m and width n
        EXAMPLES: initialBoard (3,3) = [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]
                  initialBoard (3,2) = [[Empty,Empty],[Empty,Empty],[Empty,Empty]]
    -}
    initialBoard :: (Int,Int) -> Board
    initialBoard (m,n) = replicate m (replicate n Empty)

    {- makeMove point board player
        Inserts a slot in a given board at a given point
        RETURNS: Board where a new slot has been inserted
        EXAMPLES: makeMove (1,2) [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] X = [[Empty,Empty,Empty],[Empty,Empty,(Full X)],[Empty,Empty,Empty]]
                  makeMove (0,0) [[Empty,Empty],[Empty,Empty],[Empty,Empty]] O = [[(Full O),Empty],[Empty,Empty],[Empty,Empty]]
    -}
    makeMove :: Point -> Board -> Player -> Board
    makeMove point board player = replaceBoard board point (Full player)
    
    {- replaceBoard board point slot
        Takes the current board and returns the new board with a slot inserted at a given point
        RETURNS: Board containing inserted slot
        EXAMPLES: replaceBoard [[Full X,Full X,Empty],[Full O,Empty,Empty],[Full O,Empty,Empty]] (0,2) (Full X) = [[(Full X),(Full X),(Full X)],[(Full O),Empty,Empty],[(Full O),Empty,Empty]]
                  replaceBoard [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) (Full O) = [[(Full O),Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]]
    -}
    replaceBoard :: Board -> Point -> Slot -> Board
    replaceBoard board point slot = replaceList board (fst point) (replaceList (board !! (fst point)) (snd point) slot)

     {- replaceList list int insert
        Replaces an elememnt with another in a given list
        RETURNS: The original list with one replaced element
        EXAMPLES: replaceList [Empty,(Full O),Empty] 2 (Full X) = [Empty,(Full O),(Full X)]
                  replaceList [Empty,Empty,Empty] 0 (Full O) = [(Full O),Empty,Empty]
    -}
    replaceList :: [a] -> Int -> a -> [a]
    replaceList list int insert = x ++ insert : ys
        where (x,_:ys) = splitAt int list

    {- readMove
        Checks if the input is a point, and if so returns a new point
        RETURNS: The input point with 1 subtracted from each element
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
        Checks if a point is within the board and empty
        RETURNS: True if the point exists on the board and is empty, otherwise False
        EXAMPLES: pointValid [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) = True
                  pointValid [[(Full X),Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) = False
                  pointValid [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (100,0) = False
    -}
    pointValid :: Board -> Point -> Bool
    pointValid board point = (fst point) >= 0 && (fst point) < (length board) && (snd point) >= 0 && (snd point) < (length (board !! 0)) && isEmpty board point

     {- isEmpty board point
        Checks if a slot is empty or not
        RETURNS: True if the slot is empty and false if the slot is full
        EXAMPLES: isEmpty [[Empty,Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty] (0,0) = True
                 isEmpty [[(Full O),Empty,Empty],[Empty,Empty,Empty],[Empty,Empty,Empty]] (0,0) = False
    -}
    isEmpty :: Board -> Point -> Bool
    isEmpty board point = ((board !! (fst point)) !! (snd point)) == Empty

   {-tieCount int
        Removes 1 from an Int
        RETURNS: Int - 1
        EXAMPLES: tieCount 5 = 4
   -}
    tieCount :: Int -> IO Int
    tieCount int = do
        return (int - 1)
    
    {- readInt
        Reads an input and prints it out if 
        RETURNS: The input Int
        SIDE EFFECTS: Reads one or more lines from standard input and prints strings
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
        SIDE EFFECTS: The game, updates RNG seed
    -}
    runGame :: IO ()
    runGame = do
        player <- startingPlayer
        putStrLn "\nWELCOME TO TIC TAC TOE - HOW MANY IN A ROW?\n"
        putStrLn $ "How many rows do you want?"
        m <- readInt
        putStrLn $ "How many columns do you want?"
        n <- readInt
        putStrLn $ "How many slots in a row should be required to win?"
        k <- readInt
        let count = m * n
        putStrLn "\n\n\n\n\n\n\nHow to play: when it is your turn, enter a coordinate (m,n) where m is the vertical position and n is the horizontal position\n\n"
        gameLoop count player (initialBoard (m,n)) (m,n,k)
    
    {- gameLoop count player board
       Plays the game
       SIDE EFFECTS: Reads one or more lines from standard input and prints strings
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
                    putStrLn "\n\n\n\n\n\n\n\nSmart move! :)\n"
                    gameLoop newCount newPlayer newBoard (m,n,k)
            else do
                putStrLn "Your point is out of bounds or occupied! Try again!"
                gameLoop count player board (m,n,k)
        else do
            putStrLn "It's a tie!"
    
    {-checkWin player board (m,n,k)
    Checks if a given player has won the game
    RETURNS: True or False
    EXAMPLES: checkWin X [[(Full X),Empty,Empty],[Empty,(Full X),Empty],[Empty,Empty,(Full X)]] (3,3,3) = True

    -}
    checkWin :: Player -> Board -> (Int,Int,Int) -> Bool
    checkWin player board (m,n,k) = any (all (== (Full player))) $ winCases board (m,n,k)

    {-diagonal ((x:_):rows)
    Returns the diagonal from the top left to bottom right of a board
    RETURNS: A list conatining the top left to bottom right diagonal
    VARIANT: (length rows)
    EXAMPLES: diagonal [[(Full X),Empty,Empty],[Empty,(Full X),Empty],[Empty,Empty,(Full X)]] = [(Full X),(Full X),(Full X)]
              diagonal [[Empty,Empty,(Full X)],[Empty,(Full X),Empty],[(Full X),Empty,Empty]] = [Empty,(Full X),Empty]
    -}
    diagonal :: [[a]] -> [a]
    diagonal []           = []
    diagonal ((x:_):rows) = x : diagonal (map tail rows)

    {-diags board
    Returns all left-to-right diagnoals of a board
    RETURNS: A list of all left-to-right diagonals
    EXAMPLES: diags [[(Full X),Empty,Empty],[Empty,(Full X),Empty],[Empty,Empty,(Full X)]] = [[(Full X),(Full X),(Full X)],[Empty,Empty],[Empty],[Empty,Empty],[Empty]]

    -}
    diags :: [[a]] -> [[a]]
    diags board = map diagonal (init . tails $ board) ++ tail (map diagonal (init . tails $ transpose board))

    {-winCases board (m,n,k)
    Returns all possible sequences of slots in a row that are of length k
    RETURNS: A list of all the rows, columns and diagonals on the board that have the length k
    EXAMPLES: winCases [[(Full X),Empty,Empty],[Empty,(Full X),Empty],[Empty,Empty,(Full X)]] (3,3,3) = [[Empty,Empty,(Full X)],[Empty,(Full X),Empty],[(Full X),Empty,Empty],[Empty,Empty,(Full X)],[Empty,(Full X),Empty],[(Full X),Empty,Empty],[(Full X),(Full X),(Full X)],[Empty,(Full X),Empty]]
    -}
    winCases :: Board -> (Int,Int,Int) -> [[Slot]]
    winCases board (m,n,k) =
        filter ((== k) . length) $ rows ++ cols ++ allDiags
        where
            rows = concatMap inRow $ [board !! i | i <- [0..(m-1)]]
            cols = concatMap inRow $ transpose board
            allDiags = concatMap inRow $ diags board ++ diags (map reverse board)

    {-inRow diag
    Returns all possible ways a row, column or diagonal can contain a sequence of elements.
    RETURNS: A list of lists that contains all possible ways the argument list's elements can be in a row
    EXAMPLES: inRow [Empty,(Full X),(Full X),(Full X)] = [[(Full X),(Full X),(Full X),Empty],[(Full X),(Full X),Empty],[(Full X),Empty],[Empty],[],[(Full X),(Full X),(Full X)],[(Full X),(Full X)],[(Full X)],[],[(Full X),(Full X)],[(Full X)],[],[(Full X)],[],[]]
    -}
    inRow :: [a] -> [[a]]
    inRow list = concatMap tails (map reverse(tails list))
