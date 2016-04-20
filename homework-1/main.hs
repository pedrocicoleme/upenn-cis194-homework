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
doubleEveryOther xs = reverse (doubleEverySecond (reverse xs))

-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = (+) (sum (toDigitsRev x)) (sumDigits xs)

-- Exercise 4

validate :: Integer -> Bool
validate x = (rem (sumDigits (doubleEveryOther (toDigits x))) 10) == 0

main :: IO ()
main = do
    let xs = [1,1,1,1,1,1,1]
    print (sumDigits  [16,7,12,5])
    print (doubleEveryOther [1,2,3,3])
    print (doubleEveryOther xs)
    print (toDigits 5678053)
    print (toDigits 0)
    print (toDigits (-12))

    print (validate 4012888888881881)
    print (validate 4012888888881882)
