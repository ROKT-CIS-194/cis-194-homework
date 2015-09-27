-- Exercise 5 -----------------------------------------

-- Validate a credit card number

evaluateCardNumber :: Integer -> Integer -> [Integer]
evaluateCardNumber idx num
  | even idx  = [num]
  | otherwise = [div dbl 10, mod dbl 10]
                where dbl = num * 2

integerToSingleDigitList :: Integer -> [Integer]
integerToSingleDigitList num = integerToSingleDigitList' num []
  where integerToSingleDigitList' 0 nums = nums
        integerToSingleDigitList' num nums = integerToSingleDigitList' (div num 10) ((mod num 10):nums)

luhn :: Integer -> Bool
luhn num = mod (sum (concat (zipWith evaluateCardNumber [0..] (reverse nums)))) 10 == 0
  where nums = integerToSingleDigitList num






-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs

type Peg = String
type Move = (Peg, Peg)

sparePeg :: Peg -> Peg -> Peg -> Peg -> Peg -> Peg
sparePeg a b c f t
  | f == a = if t == b then c else b
  | f == b = if t == a then c else a
  | f == c = if t == a then b else a

hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c f t
  | n < 1 = []
  | n == 1 = [(f, t)]
  | n == 2 = [(f, m), (f, t), (m, t)]
  | otherwise = concat [(hanoi' (n - 1) a b c f m), [(f, t)], (hanoi' (n - 1) a b c m t)]
  where m = sparePeg a b c f t

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c = hanoi' n a b c a b
