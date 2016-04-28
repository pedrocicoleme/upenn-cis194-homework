-- Exercise 5

{- To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,
1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage. -}

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
--hanoi 2 a b c = [(a, c), (a, b), (c, b)]
--hanoi 3 a b c = [(a, b), (a, c), (b, c), (a, b), (c, a), (c, b), (a,b)]
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 a b _ _ = [(a, b)]
hanoi4 2 a b c _ = [(a, c), (a, b), (c, b)]
--hanoi4 3 a b c d = [(a, d), (a, c), (a, b), (c, b), (d, b)]
hanoi4 n a b c d = (hanoi4 (n-2) a d c b) ++ [(a, c), (a, b), (c, b)] ++ (hanoi4 (n-2) d b a c)

-- based on wikipedia
hanoi4opt :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4opt 0 _ _ _ _ = []
hanoi4opt 1 a b _ _ = [(a, b)]
hanoi4opt 2 a b c _ = [(a, c), (a, b), (c, b)]
--hanoi4opt 3 a b c d = [(a, d), (a, c), (a, b), (c, b), (d, b)]
hanoi4opt n a b c d = (hanoi4 (n-2) a d c b) ++ [(a, c), (a, b), (c, b)] ++ (hanoi4 (n-2) d b a c)

main :: IO ()
main = do
    print (hanoi 2 "a" "b" "c") -- should be [("a","c"), ("a","b"), ("c","b")]
    print (hanoi 3 "a" "b" "c") -- should be [(a, b), (a, c), (b, c), (a, b), (c, a), (c, b), (a,b)]
    print (length (hanoi 15 "a" "b" "c")) -- should be 32767
    print (hanoi4 2 "a" "b" "c" "d")
    print (hanoi4 3 "a" "b" "c" "d")
    print (hanoi4 4 "a" "b" "c" "d")
    print (length (hanoi4 15 "a" "b" "c" "d")) -- should be 32767
