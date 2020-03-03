module Tests where
import Test.HUnit
import TTT.Game

testPointValid :: Test
testPointValid = TestLabel "pointValid" $ TestList
    [ TestLabel "Should give True when inside bounds and slot is empty" $ TestCase $ do
        let
            test1 :: Bool
            test1 = (TTT.Game.pointValid (TTT.Game.initialBoard (3,3)) (0,0))

            test2 :: Bool
            test2 = (TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (4,0))

        True @=? test1
        True @=? test2
  , TestLabel "Should return False when out of bounds" $ TestCase $ do
        let
            test3 :: Bool
            test3 = (TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (5,0))

            test4 :: Bool
            test4 = (TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (0,3))

        False @=? test3
        False @=? test4
  , TestLabel "Should return False when inside bounds and slot is full" $ TestCase $ do
        let
            test5 :: Bool
            test5 = (TTT.Game.pointValid (TTT.Game.makeMove (1,1) (TTT.Game.initialBoard (5,4)) TTT.Game.X) (1,1))

        False @=? test5

    ]

testCheckWin :: Test
testCheckWin = TestLabel "checkWin" $ TestList
    [ TestLabel "Should return True if there is a diagonal that has the length k = 3" $ TestCase $ do
        let
            test1 :: Bool
            test1 = (TTT.Game.checkWin X (TTT.Game.makeMove (0,0) (TTT.Game.makeMove (1,1) (TTT.Game.makeMove (2,2) (TTT.Game.initialBoard (4,5)) TTT.Game.X) TTT.Game.X) TTT.Game.X) (4,5,3))

            test2 :: Bool
            test2 = (TTT.Game.checkWin X (TTT.Game.makeMove (0,4) (TTT.Game.makeMove (1,3) (TTT.Game.makeMove (2,2) (TTT.Game.initialBoard (4,5)) TTT.Game.X) TTT.Game.X) TTT.Game.X) (4,5,3))

        True @=? test1
        True @=? test2

    , TestLabel "Should return True/False if there is/isn't a column that has the length k = 3" $ TestCase $ do
        let
            test3 :: Bool
            test3 = (TTT.Game.checkWin X (TTT.Game.makeMove (1,1) (TTT.Game.makeMove (2,1) (TTT.Game.makeMove (3,1) (TTT.Game.initialBoard (4,5)) TTT.Game.X) TTT.Game.X) TTT.Game.X) (4,5,3))

            test4 :: Bool
            test4 = (TTT.Game.checkWin X (TTT.Game.makeMove (0,1) (TTT.Game.makeMove (2,1) (TTT.Game.makeMove (3,1) (TTT.Game.initialBoard (4,5)) TTT.Game.X) TTT.Game.X) TTT.Game.X) (4,5,3))

        True @=? test3
        False @=? test4
    
    , TestLabel "Should return True if there is a row that has the length k = 3" $ TestCase $ do
        let
            test5 :: [Bool]
            test5 = [(TTT.Game.checkWin X (TTT.Game.makeMove (1,1) (TTT.Game.makeMove (1,2) (TTT.Game.makeMove (1,3) (TTT.Game.initialBoard (i,5)) TTT.Game.X) TTT.Game.X) TTT.Game.X) (i,5,3)) | i <- [4..10]]

        [True, True, True, True, True, True, True] @=? test5

    ]

testaa = TestCase (assertEqual "for TTT.Game.makeMove (1,2) [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] TTT.Game.X," [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,(Full X)],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] (TTT.Game.makeMove (1,2) [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] TTT.Game.X))

tests = TestList [TestLabel "pointValid tests" testPointValid, TestLabel "checkWin tests" testCheckWin]

runtests = runTestTT tests