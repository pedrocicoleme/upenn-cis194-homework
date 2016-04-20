-- Double the value of every second digit beginning from the right.
-- That is, the last digit is unchanged; the second-to-last digit is doubled;
-- the third-to-last digit is unchanged; and so on. For example,
-- [1,3,8,6] becomes [2,3,16,6].

doubleEverySecond :: [Int] -> [Int]
doubleEverySecond [] = []
doubleEverySecond (x:y:ys) = (x*2) : y : doubleEverySecond ys

main :: IO ()
main = do
    print (doubleEverySecond ([1,1,1,1,1,1]))
