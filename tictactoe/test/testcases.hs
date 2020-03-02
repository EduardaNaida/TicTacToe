module Tests where
import Test.HUnit
import TTT.Game

test1 = TestCase (assertEqual "for TTT.Game.makeMove (1,2) [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] TTT.Game.X," [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,(Full X)],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] (TTT.Game.makeMove (1,2) [[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty],[TTT.Game.Empty,TTT.Game.Empty,TTT.Game.Empty]] TTT.Game.X))
test2 = TestCase (assertEqual "for TTT.Game.pointValid (TTT.Game.initialBoard (3,3)) (0,0)" True (TTT.Game.pointValid (TTT.Game.initialBoard (3,3)) (0,0)))
test3 = TestCase (assertEqual "for TTT.Game.pointValid (TTT.Game.initialBoard (5,1) (5,0)" False (TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (5,0)))
test4 = TestCase (assertEqual "for TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (4,0)" True (TTT.Game.pointValid (TTT.Game.initialBoard (5,1)) (4,0)))
    
tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2, TestLabel "test3" test3, TestLabel "test4" test4]

runtests = runTestTT tests