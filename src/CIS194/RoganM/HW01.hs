evaluateCardNumber :: Integer -> Integer -> [Integer]
evaluateCardNumber idx num
  | even idx  = [num]
  | otherwise = [div dbl 10, mod dbl 10]
                where dbl = num * 2

integerToSingleDigitList :: Integer -> [Integer]
integerToSingleDigitList num = integerToSingleDigitList' num []
  where integerToSingleDigitList' 0 nums = nums
        integerToSingleDigitList' num nums = integerToSingleDigitList' (div num 10) ((mod num 10):nums)

validCreditCard :: Integer -> Bool
validCreditCard num = mod (sum (concat (zipWith evaluateCardNumber [0..] (reverse nums)))) 10 == 0
                      where nums = integerToSingleDigitList num
