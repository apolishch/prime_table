module Lib
    ( getInputAndGenerate
    ) where

generateMultiplyAndPrint :: Int -> IO ()
generateMultiplyAndPrint numPrimes | numPrimes <= 0 = putStrLn "Please enter a valid integer number of primes greater than 0" >> getInputAndGenerate
generateMultiplyAndPrint _ = putStrLn "bar"

getInput :: IO String
getInput = putStrLn "Please enter the number of primes you wish to generate and multiply" >> getLine

getInputAndGenerate :: IO ()
getInputAndGenerate = getInput >>= \number -> generateMultiplyAndPrint (read number :: Int)
