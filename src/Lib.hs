module PrimeSieve (getInputAndGenerate) where

  sieve :: [Int]
  sieve = [1]

  generatePrimes :: Int -> [Int]
  generatePrimes count = take count sieve

  generateTable :: [Int] -> [[Int]]
  generateTable [] = []
  generateTable as = [as]

  generateMultiplyAndPrint :: Int -> IO ()
  generateMultiplyAndPrint numPrimes | numPrimes <= 0 = putStrLn "Please enter a valid integer number of primes greater than 0" >> getInputAndGenerate
                                     | otherwise = take numPrimes sieve

  getInput :: IO String
  getInput = putStrLn "Please enter the number of primes you wish to generate and multiply" >> getLine

  getInputAndGenerate :: IO ()
  getInputAndGenerate = getInput >>= \number -> generateMultiplyAndPrint (read number :: Int)
