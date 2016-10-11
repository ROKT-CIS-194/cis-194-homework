module CIS194.ClaudioN.HW03Tests where

import CIS194.ClaudioN.HW03
import CIS194.Testing

import Data.List (foldl')

runStateUpdates :: [(String, Int)] -> State
runStateUpdates = foldl' (\x (var,val) -> extend x var val) empty

-- Exercise 1 -----------------------------------------

testState :: ([(String, Int)], (String, Int)) -> Bool
testState (inputs, (oVar, oVal)) = runStateUpdates inputs oVar == oVal

ex1Tests :: [Test]
ex1Tests = [ Test "state: empty" testState
             [ ([], ("X", 0)) ]
           , Test "state: set" testState
             [ ([("X", 1)], ("X", 1))
             , ([("Y", 1)], ("X", 0)) ]
           , Test "state: multiple updates" testState
             [ ([("X", 1), ("X", 2)], ("X", 2)) ]
           ]

-- Exercise 2 -----------------------------------------

testEvalE :: ([(String, Int)], Expression, Int) -> Bool
testEvalE (inputs, expr, result) = evalE (runStateUpdates inputs) expr == result

ex2Tests :: [Test]
ex2Tests = [ Test "evalE: variables" testEvalE
             [ ([], Var "X", 0)
             , ([("X", 1)], Var "X", 1)
             ]
           , Test "evalE: values" testEvalE
             [ ([], Val 0, 0)
             , ([], Val 1, 1)
             ]
           , Test "evalE: arithmetic" testEvalE
             [ ([], Op (Val 2) Plus (Val 3), 5)
             , ([], Op (Val 2) Minus (Val 3), (-1))
             , ([], Op (Val 2) Times (Val 3), 6)
             , ([], Op (Val 20) Divide (Val 3), 6)
             ]
           , Test "evalE: comparison" testEvalE
             [ ([], Op (Val 2) Gt (Val 3), 0)
             , ([], Op (Val 3) Gt (Val 2), 1)
             , ([], Op (Val 2) Lt (Val 3), 1)
             , ([], Op (Val 3) Lt (Val 2), 0)
             , ([], Op (Val 3) Eql (Val 2), 0)
             , ([], Op (Val 2) Eql (Val 2), 1)
             ]
           ]

-- Exercise 3 -----------------------------------------

testDesugar :: (Statement, DietStatement) -> Bool
testDesugar (stmt, dstmt) = desugar stmt == dstmt

ex3Tests :: [Test]
ex3Tests = [ Test "desugar: one to one" testDesugar
             [ (Assign "X" (Var "X"), DAssign "X" (Var "X"))
             , (If (Var "X") Skip Skip, DIf (Var "X") DSkip DSkip)
             , (While (Var "X") Skip, DWhile (Var "X") DSkip)
             , ( Sequence (Assign "X" (Val 1)) Skip
               , DSequence (DAssign "X" (Val 1)) DSkip)
             , (Skip, DSkip)
             ]
           , Test "desugar: sugar" testDesugar
             [ (Incr "X", DAssign "X" (Op (Var "X") Plus (Val 1)))
             , ( For Skip (Var "X") Skip (Assign "X" (Val 1))
               , DSequence DSkip (DWhile (Var "X")
                                  (DSequence (DAssign "X" (Val 1)) DSkip))
               )
             ]
           ]

-- Exercise 4 -----------------------------------------

testStatement :: ([(String, Int)], Statement, (String, Int)) -> Bool
testStatement (inputs, stmt, (oVar, oVal)) =
  evalSimple (runStateUpdates inputs) (desugar stmt) oVar == oVal

ex4Tests :: [Test]
ex4Tests = [ Test "eval: assign" testStatement
             [ ([], Assign "X" (Val 1), ("X", 1))
             , ([("X", 2)], (Assign "X" (Op (Var "X") Times (Val 2))), ("X", 4))
             ]
           , Test "eval: incr" testStatement
             [ ([], Incr "X", ("X", 1))
             , ([("X", 2)], Incr "X", ("X", 3))
             ]
           , Test "eval: if" testStatement
             [ ([("X", 1)], If (Var "X") (Assign "Y" (Var "X")) Skip
               , ("Y", 1))
             , ([("X", 0), ("Y", 2)], If (Var "X") (Assign "Y" (Var "X")) Skip
               , ("Y", 2))
             ]
           , Test "eval: while" testStatement
             [ ([], While (Op (Var "X") Lt (Val 10))
                          (Assign "X" (Op (Var "X") Plus (Val 1))),
                ("X", 10))
             ]
           , Test "eval: for" testStatement
             [ ([], For (Assign "X" (Val 1))
                        (Op (Var "X") Lt (Val 10))
                        (Incr "X")
                        (Assign "Y" (Var "X"))
               , ("Y", 9))
             ]
           , Test "eval: sequence" testStatement
             [ ([], Sequence (Assign "X" (Val 1)) (Assign "Y" (Var "X"))
               , ("Y", 1))
             , ([], Sequence (Assign "X" (Val 1)) (Assign "X" (Op (Var "X") Plus (Val 1)))
               , ("X", 2))
             ]
           , Test "eval: skip" testStatement
             [ ([], Skip, ("X", 0))
             ]
           ]

-- Example Programs -----------------------------------------

testExample :: Statement -> (String, String) -> (Int, Int) -> Bool
testExample prog (vi, vo) (i, o) =
  evalSimple (extend empty vi i) (desugar prog) vo == o

progTests :: [Test]
progTests = [ Test "examples: factorial" (testExample factorial ("In", "Out"))
              (zip [0..] [1, 1, 2, 6, 24, 120, 720, 5040])
            , Test "examples: squareRoot" (testExample squareRoot ("A", "B"))
              [(1, 1), (4, 2), (6, 2), (16, 4), (99, 9), (100, 10)]
            , Test "exmaples: fibonacci" (testExample fibonacci ("In", "Out"))
              (zip [0..] [1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89])
            ]

-- All Tests ------------------------------------------

allTests :: [Test]
allTests = concat [ ex1Tests
                  , ex2Tests
                  , ex3Tests
                  , ex4Tests
                  , progTests
                  ]
