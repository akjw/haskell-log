module Main where

import Test.Hspec
import Test.QuickCheck
import Lib

main :: IO ()
main = hspec $
    describe "fillInGuess" $ do
      context "using 'aria' as test case" $ do
        let puzzle    = Puzzle "aria" [Nothing,  Nothing, Nothing, Nothing] ""
        it "fills in single occurence of correct letter" $ do
          -- let puzzle    = Puzzle "aria" [Nothing,  Nothing, Nothing, Nothing] ""
          let updatedPuzzle = Puzzle "aria" [Nothing, Nothing, Just 'i', Nothing] ['i']
          fillInCharacter puzzle 'i' `shouldBe` updatedPuzzle

        it "fills in multiple occurrences of correct letter" $ do
          -- let puzzle    = Puzzle "aria" [Nothing,  Nothing, Nothing, Nothing] ""
          let updatedPuzzle = Puzzle "aria" [Just 'a', Nothing, Nothing, Just 'a'] ['a']
          fillInCharacter puzzle 'a' `shouldBe` updatedPuzzle

        it "does not fill in for incorrect letter" $ do
          -- let puzzle    = Puzzle "aria" [Nothing,  Nothing, Nothing, Nothing] ""
          let updatedPuzzle = Puzzle "aria" [Nothing, Nothing, Nothing, Nothing] ['z']
          fillInCharacter puzzle 'z' `shouldBe` updatedPuzzle

      describe "handleGuess" $ do
        context "using 'aria' as test case" $ do
          let puzzle    = Puzzle "aria" [Nothing,  Nothing, Nothing, Nothing] ""
          it "handles single occurence of correct letter" $ do
              updatedPuzzle <- handleGuess puzzle 'i'
              updatedPuzzle `shouldBe` (Puzzle "aria" [Nothing, Nothing, Just 'i', Nothing] ['i'])

          it "handles multiple occurrences of correct letter" $ do
              updatedPuzzle <- handleGuess puzzle 'a'
              updatedPuzzle `shouldBe` (Puzzle "aria" [Just 'a', Nothing, Nothing, Just 'a'] ['a'])

          it "handles unchanged puzzle for incorrect letter; adds letter to guesses" $ do
              updatedPuzzle <- handleGuess puzzle 'z'
              updatedPuzzle `shouldBe` (Puzzle "aria" [Nothing, Nothing, Nothing, Nothing] ['z'])

          it "handles repeat guess" $ do
              let puzzle    = Puzzle "aria" [Just 'a',  Nothing, Nothing, Just 'a'] ['a']
              updatedPuzzle <- handleGuess puzzle 'a'
              updatedPuzzle `shouldBe` (Puzzle "aria" [Just 'a', Nothing, Nothing, Just 'a'] ['a'])

