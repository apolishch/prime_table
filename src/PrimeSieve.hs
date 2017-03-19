{-# LANGUAGE CPP #-}
module PrimeSieve (
  getInputAndGenerate
    ,sieve
    ,generatePrimes
    ,generateTable
    ,displayTable
    ,generateMultiplyAndPrint
    ,getInput
  ) where

  sieve :: [Integer]
  sieve = [1]

  generatePrimes :: Int -> [Integer]
  generatePrimes count = take count sieve

  generateTable :: [Integer] -> [[Integer]]
  generateTable [] = []
  generateTable as = [as]

  displayTable :: [[Integer]] -> IO ()
  displayTable as = putStrLn "foo"

  generateMultiplyAndPrint :: Int -> IO ()
  generateMultiplyAndPrint numPrimes | numPrimes <= 0 = putStrLn "Please enter a valid integer number of primes greater than 0" >> getInputAndGenerate
                                     | otherwise = (displayTable . generateTable . take numPrimes) sieve

  getInput :: IO String
  getInput = putStrLn "Please enter the number of primes you wish to generate and multiply" >> getLine

  getInputAndGenerate :: IO ()
  getInputAndGenerate = getInput >>= \number -> generateMultiplyAndPrint (read number :: Int)
