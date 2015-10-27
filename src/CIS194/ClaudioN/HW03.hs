module CIS194.ClaudioN.HW03 where

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
extend st var i = st'
  where st' lvar
          | (var == lvar) = i
          | otherwise = st lvar

empty :: State
empty _ = 0  -- hmmm...

-- Exercise 2 -----------------------------------------

b2i :: Bool -> Int
b2i False = 0
b2i True  = 1

evalE :: State -> Expression -> Int
evalE st (Var a) = st a
evalE st (Val i) = i
evalE st (Op ex bop ey) = op x y
  where op = case bop of
               Plus   -> (+)
               Minus  -> (-)
               Times  -> (*)
               Divide -> (div)
               Gt     -> (b2i .) . (>)
               Ge     -> (b2i .) . (>=)
               Lt     -> (b2i .) . (<)
               Le     -> (b2i .) . (<=)
               Eql    -> (b2i .) . (==)
        x = evalE st ex
        y = evalE st ey

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s ex) = DAssign s ex
desugar (If ex stmt1 stmt2) = DIf ex (desugar stmt1) (desugar stmt2)
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (While ex stmt) = DWhile ex (desugar stmt)
desugar (For astmt ex istmt stmt) = DSequence (desugar astmt)
                                              (DWhile ex (desugar (Sequence stmt istmt)))
desugar (Sequence stmt1 stmt2) = DSequence (desugar stmt1) (desugar stmt2)
desugar Skip = DSkip

-- Exercise 4 -----------------------------------------
truthy :: Int -> Bool
truthy 0 = False
truthy _ = True

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s ex) = extend st s (evalE st ex)
evalSimple st (DIf ex stmt1 stmt2)
  | truthy $ evalE st ex = evalSimple st stmt1
  | otherwise = evalSimple st stmt2
evalSimple st (DWhile ex stmt)
  | truthy $ evalE st ex = foldl evalSimple st [stmt, (DWhile ex stmt)]
  | otherwise = st
evalSimple st (DSequence stmt1 stmt2) = foldl evalSimple st [stmt1, stmt2]
evalSimple st DSkip = st

run :: State -> Statement -> State
run st stmt = evalSimple st (desugar stmt)

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
