module Lib
    ( someFunc
    ) where

generateMultiplyAndPrint :: Int -> IO ()
generateMultiplyAndPrint 0 = putStrLn "foo"
generateMultiplyAndPrint _ = putStrLn "bar"

getInput :: IO String
getInput = putStrLn "Please enter the number of primes you with to generate and multiply" >> getLine

someFunc :: IO ()
someFunc = getInput >>= \number -> generateMultiplyAndPrint (read number :: Int)
