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

-- based on wikipedia - https://en.wikipedia.org/wiki/Tower_of_Hanoi#With_four_pegs_and_beyond
hanoi4opt :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4opt 0 _ _ _ _ = []
hanoi4opt n a b c d =
    (hanoi4opt (n-(round (sqrt (fromIntegral(2*n+1)))) + 1) a d c b) ++
    (hanoi (((round (sqrt (fromIntegral(2*n+1)))) - 1)) a b c) ++
    (hanoi4opt (n-(round (sqrt (fromIntegral(2*n+1)))) + 1) d a c b)

hanoi5opt :: Integer -> Peg -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi5opt 0 _ _ _ _ _ = []
hanoi5opt n a b c d e =
    (hanoi5opt (n-(round (sqrt (fromIntegral(2*n+1)))) + 1) a d c b e) ++
    (hanoi4opt (((round (sqrt (fromIntegral(2*n+1)))) - 1)) a b c e) ++
    (hanoi5opt (n-(round (sqrt (fromIntegral(2*n+1)))) + 1) d a c b e)

main :: IO ()
main = do
    print (hanoi 2 "a" "b" "c") -- should be [("a","c"), ("a","b"), ("c","b")]
    print (hanoi 3 "a" "b" "c") -- should be [(a, b), (a, c), (b, c), (a, b), (c, a), (c, b), (a,b)]
    print (length (hanoi 15 "a" "b" "c")) -- should be 32767
    print (hanoi4 2 "a" "b" "c" "d")
    print (hanoi4 3 "a" "b" "c" "d")
    print (hanoi4 4 "a" "b" "c" "d")
    print (length (hanoi4 15 "a" "b" "c" "d"))
    print (hanoi4opt 2 "a" "b" "c" "d")
    print (hanoi4opt 3 "a" "b" "c" "d")
    print (hanoi4opt 4 "a" "b" "c" "d")
    print (length (hanoi4opt 15 "a" "b" "c" "d")) -- should be 129 optimally
    print (length (hanoi5opt 15 "a" "b" "c" "d" "e")) -- should be 129 optimally
