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
  sieve = 2:3:sieveOfErastothenes (tail sieve) [5,7..]
    where
      sieveOfErastothenes (p:ps) ds = lesser ++ sieveOfErastothenes ps [x | x <- ps1, x `mod` p /= 0]
        where (lesser, (_:ps1)) = span (< p^2) ds

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
