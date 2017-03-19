module PrimeSieveSpec (spec) where

  import PrimeSieve
  import Test.Hspec
  import Test.Hspec.QuickCheck
  import Test.QuickCheck
  import Math.NumberTheory.Primes.Testing

  genSafeInteger :: Gen Int
  genSafeInteger = choose (1, 20000)

  first25 :: [Integer]
  first25 = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

  spec :: Spec
  spec = do
    describe "PrimeSieve.sieve" $ do
      it "returns the right list" $ do
        take 25 sieve `shouldBe` first25

    describe "PrimeSieve.generatePrimes" $ do
      it "returns the right list" $ do
        generatePrimes 25 `shouldBe` first25

      prop "always returns a list where every element is prime" $
        forAll genSafeInteger $ \i -> foldl (\memo prime -> (isCertifiedPrime prime) && memo) True (generatePrimes i)


    describe "PrimeSieve.generateTable" $ do
      it "generates the right table" $ do
        generateTable (take 3 first25) `shouldBe` [[2,3,5],[4,6,10],[6,9,15]]

      prop "always returns a list of the correct length" $
        forAll genSafeInteger $ \i -> length (generateTable $ generatePrimes i) == i
