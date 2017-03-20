module PrimeSieve (
  getInputAndGenerate
  ,sieve
  ,generatePrimes
  ,generateTable
  ,generateField
  ,generateFields
  ,constructLine
  ) where

  import Data.List

  sieve :: [Integer]
  sieve = 2:3:sieveOfErastothenes (tail sieve) [5,7..]
    where
      sieveOfErastothenes (p:ps) ds = lesser ++ sieveOfErastothenes ps [x | x <- ps1, x `mod` p /= 0]
        where (lesser, (_:ps1)) = span (< p^2) ds

  generatePrimes :: Int -> [Integer]
  generatePrimes count = take count sieve

  generateTable :: [Integer] -> [[Integer]]
  generateTable as = map (\int -> map (int*) as) as

  generateField :: Integer -> String
  generateField a = show a ++ (replicate (7 - (length $ show a)) ' ')

  generateFields :: [Integer] -> String
  generateFields as = "|" ++ intercalate " | " (map generateField as) ++ " |"

  constructLine :: Maybe Integer -> [Integer] -> String
  constructLine Nothing as = "|       " ++ generateFields as
  constructLine (Just a) as = "|" ++ generateField a ++ generateFields as

  displayRows :: [Integer] -> [[Integer]] -> IO ()
  displayRows (b:bs) (a:as) = putStrLn (constructLine (Just b) a) >> displayRows bs as

  displayTableWithBorders :: [Integer] -> [[Integer]] -> IO ()
  displayTableWithBorders bs as = do
    putStrLn $ constructLine Nothing bs
    displayRows bs as   

  displayTable :: [[Integer]] -> IO ()
  displayTable as = displayTableWithBorders (generatePrimes (length $ head as)) as

  generateMultiplyAndPrint :: Int -> IO ()
  generateMultiplyAndPrint numPrimes | numPrimes <= 0 = putStrLn "Please enter a valid integer number of primes greater than 0" >> getInputAndGenerate
                                     | otherwise = (displayTable . generateTable . take numPrimes) sieve

  getInput :: IO String
  getInput = putStrLn "Please enter the number of primes you wish to generate and multiply" >> getLine

  getInputAndGenerate :: IO ()
  getInputAndGenerate = getInput >>= \number -> generateMultiplyAndPrint (read number :: Int)
