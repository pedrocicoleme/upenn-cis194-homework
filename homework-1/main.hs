-- Double the value of every second digit beginning from the right.
-- That is, the last digit is unchanged; the second-to-last digit is doubled;
-- the third-to-last digit is unchanged; and so on. For example,
-- [1,3,8,6] becomes [2,3,16,6].

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond [] = []
doubleEverySecond (x:y:ys) = (x*2) : y : doubleEverySecond ys

-- Add the digits of the doubled values and the undoubled digits
-- from the original number. For example, [2,3,16,6] becomes
-- 2+3+1+6+6 = 18.

-- sumList :: (Num a) => [a] -> a
-- sumList [] = 0
-- sumList (x:xs) = (+) x (sumList xs)

sumList :: (Num a) => [a] -> a
sumList xs = foldr (+) 0 xs

main :: IO ()
main = print (sumList(doubleEverySecond [1,1,1,1,1,1]))
