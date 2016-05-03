{-# OPTIONS_GHC -Wall #-}
-- module LogAnalysis where

import Log

-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage xs = parseMessage1 (words xs)

parseMessage1 :: [String] -> LogMessage
parseMessage1 ("I":t:xs) = LogMessage Info (read t :: TimeStamp) (unwords xs)
parseMessage1 ("W":t:xs) = LogMessage Warning (read t :: TimeStamp) (unwords xs)
parseMessage1 ("E":e:t:xs) = LogMessage (Error (read e :: Int)) (read t :: TimeStamp) (unwords xs)
parseMessage1 xs = Unknown (unwords xs)

parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

-- main :: IO ()
-- main =
--     print (parseMessage "I 562 help help")
