-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0  = []
    | x < 10  = [x]
    | otherwise = (rem x 10) : toDigitsRev (div x 10)

toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- Exercise 2

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond (x:[]) = [x]
doubleEverySecond (x:y:ys) = x : (y*2) : doubleEverySecond ys

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse(doubleEverySecond (reverse xs))


-- Add the digits of the doubled values and the undoubled digits
-- from the original number. For example, [2,3,16,6] becomes
-- 2+3+1+6+6 = 18.

-- sumList :: (Num a) => [a] -> a
-- sumList [] = 0
-- sumList (x:xs) = (+) x (sumList xs)

sumList :: (Num a) => [a] -> a
sumList xs = foldr (+) 0 xs

main :: IO ()
main = do
    let xs = [1,1,1,1,1,1,1]
    print (doubleEveryOther [1,2,3,3])
    print (doubleEveryOther xs)
    print (sumList(doubleEveryOther xs))
    print (toDigits 5678053)
    print (toDigits 0)
    print (toDigits (-12))
