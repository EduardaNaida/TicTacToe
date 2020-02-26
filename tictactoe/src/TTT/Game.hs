module TTT.Game where

    import Data.Char
    import System.Random
    import Text.Read
    import Data.List
    
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
     The player can ither be X or O.
   -}
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
    

    
    --Turn
    
    --data Turn = PlayerX | PlayerO deriving(Show, Eq)
    -- ^^ behövs inte enligt Love

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
    Takes the players as a data type and returns them as a IO action
    Returns: IO X or IO O
    Side-effect: Stores the player in a IO
    -}
    nextPlayer :: Player -> IO Player
    nextPlayer X = do return O
    nextPlayer O = do return X
    
    {-playerInsert Player
    Takes a Players move and returns it as a slot in the game
    Returns: Full x or Full O
    -}
    playerInsert :: Player -> Slot
    playerInsert X = Full X 
    playerInsert O = Full O
    
    {-initialBoard
    Creates a list of 3 with three elements who are Empty inside of it. It is the base of the gameplan
    Returns: a board
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
    
    Returns: 
    Ex: 
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

     {- replaceList list int insert
    Used for updating a list with a new element: insert. Takes the insert and switch the place with the element int the list after
    the int inputted.
    Returns: 
    Ex: replaceList [1,2,3] 2 4 == [1,2,4]
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

    pointValid :: Board -> Point -> IO  Bool
    pointValid board point = if (fst point) >= 0 && (fst point) < 3 && (snd point) >= 0 && (snd point) < 3 && isEmpty board point then do return True 
        else do return False

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
            valid <- pointValid board point
            if valid then do
                    newBoard <- makeMove point board player
            --win <- checkWin point newBoard
            -- använd makeMove för nytt bräde. Om invalid, kör samma gameLoop igen
            -- om valit, kolla vinst; om ingen vinst, kör gameLoop på det nya brädet, dekrementera movesLeft, och byt spelare
                    newPlayer <- nextPlayer player
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
