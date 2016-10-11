module CIS194.NavinK.HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s i = f
  where f x
          | x == s = i
          | otherwise = st x

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st expr = case expr of
  Var s -> st s
  Val i -> i
  Op expr1 bop expr2 -> case bop of
    Plus -> evalE st expr1 + evalE st expr2
    Minus -> evalE st expr1 - evalE st expr2
    Times -> evalE st expr1 * evalE st expr2
    Divide -> evalE st expr1 `div` evalE st expr2
    Gt -> case evalE st expr1 > evalE st expr2 of
      True -> 1
      False -> 0
    Ge -> case evalE st expr1 >= evalE st expr2 of
      True -> 1
      False -> 0
    Lt -> case evalE st expr1 < evalE st expr2 of
      True -> 1
      False -> 0
    Le -> case evalE st expr1 <= evalE st expr2 of
      True -> 1
      False -> 0
    Eql -> case evalE st expr1 == evalE st expr2 of
      True -> 1
      False -> 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar stmt = case stmt of
  Assign str expr -> DAssign str expr
  Incr str -> DAssign str (Op (Var str) Plus (Val 1))
  If expr stmt1 stmt2 -> DIf expr (desugar stmt1) (desugar  stmt2)
  While expr stmt1 -> DWhile expr (desugar stmt1)
  For stmt1 expr stmt2 stmt3 -> DSequence (desugar stmt1)
                                          (DWhile expr (DSequence (desugar stmt3)
                                                  (desugar stmt2)))
  Sequence stmt1 stmt2 -> DSequence (desugar stmt1) (desugar stmt2)
  Skip -> DSkip

-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State

evalSimple state (DAssign str1 expr) = f
  where f str
          | str == str1 = evalE state expr
          | otherwise = state str

evalSimple state (DIf expr dietStmt1 dietStmt2)
  | evalE state expr == 0 = (evalSimple state dietStmt2)
  | otherwise = (evalSimple state dietStmt1)

evalSimple state (DWhile expr dietStmt)
  | evalE state expr == 0 = state
  | otherwise = evalSimple (evalSimple state dietStmt)
                              (DWhile expr dietStmt)

evalSimple state (DSequence dietStmt1 dietStmt2) =
  evalSimple (evalSimple state dietStmt1) dietStmt2

evalSimple state DSkip = state

run :: State -> Statement -> State
run state stmt = evalSimple state (desugar stmt)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
