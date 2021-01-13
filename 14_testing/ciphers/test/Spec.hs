module Main where

import Test.Hspec
import Test.QuickCheck
import Lib

genString :: Gen String
genString = listOf1 genAlphaChar
  where genAlphaChar :: Gen Char
        genAlphaChar = elements ['a'..'z']

genCaesar :: Gen (String, Int)
genCaesar = do
  str      <- genString
  shiftNum <- arbitrary
  return (str, shiftNum)

genVigenere:: Gen (String, String)
genVigenere = do
  str <- genString
  key <- genString
  return (str, key)

prop_matching_caesar :: Property
prop_matching_caesar =
  forAll genCaesar
  (\(str, shiftNum)-> (unCaesar (caesar str shiftNum) shiftNum) == str)

prop_matching_vigenere:: Property
prop_matching_vigenere =
  forAll genVigenere
  (\(str, key)-> (unVigenere (vigenere str key) key) == str)

main :: IO ()
main = do
  putStrLn "matching caesar-uncaesar"
  quickCheck prop_matching_caesar
  putStrLn "matching vigenere-unVigenere"
  quickCheck prop_matching_vigenere


