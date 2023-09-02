module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Folder


main :: IO ()
main = hspec $ do
    describe "Prelude.head" $ do
        it "returns the first element of a list" $ do
            head [23 ..] `shouldBe` (23 :: Int)
        
        it "returns the first element of an *arbitrary* list" $
            property $ \x xs -> head (x:xs) == (x :: Int)
        
        it "throws an exception if used with an empty list" $ do
            evaluate (head []) `shouldThrow` anyException

    describe "Folder" $ do
        it "should return reversed list" $ do
            rev [1, 2, 3] `shouldBe` ([3, 2, 1] :: [Int])

        it "should return properly prefixed list" $ do
            prefixes [1, 2, 3] `shouldBe` ([[1], [1, 2], [1, 2, 3]] :: [[Int]])
        