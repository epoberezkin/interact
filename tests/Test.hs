{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State
import Data.ByteString.Char8 (pack)
import System.IO.Interact
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Main (withStdin)

main :: IO ()
main = hspec do
  describe "repl" do
    it "[String] -> [String]" $
      ["ab", "cdf", "", "efgh"] #-> repl (map doubleString)
        -># ["abab", "cdfcdf", "", "efghefgh"]
    it "String -> String" $
      ["ab", "cdf", "", "efgh"] #-> repl doubleString
        -># ["abab", "cdfcdf", "", "efghefgh"]
    it "String -> Maybe String" $
      ["ab", "cdf", "", "efgh"] #-> repl maybeDoubleString
        -># ["abab", "cdfcdf"]
    it "String -> Either String String" $
      ["ab", "cdf", "", "efgh"] #-> repl eitherDoubleString
        -># ["abab", "cdfcdf", "bye"]
    it "[Int] -> [Int]" $
      ["2", "17", "0", "123"] #-> repl (map doubleInt)
        -># ["4", "34", "0", "246"]
    it "[Int] -> [Int] (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> repl (map doubleInt)
        -># ["4", "34", "0", "246"]
    it "Int -> Int" $
      ["2", "17", "0", "123"] #-> repl doubleInt
        -># ["4", "34", "0", "246"]
    it "Int -> Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> repl doubleInt
        -># ["4", "Invalid input", "34", "0", "246"]
    it "Int -> Maybe Int" $
      ["2", "17", "0", "123"] #-> repl maybeDoubleInt
        -># ["4", "34"]
    it "Int -> Maybe Int (invalid input) " $
      ["2", "abc", "17", "0", "123"] #-> repl maybeDoubleInt
        -># ["4", "Invalid input", "34"]
    it "Int -> Either String Int" $
      ["2", "17", "0", "123"] #-> repl eitherDoubleInt
        -># ["4", "34", "bye"]
    it "Int -> Either String Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> repl eitherDoubleInt
        -># ["4", "Invalid input", "34", "bye"]
  describe "repl'" do
    it "0 -> Int -> Int" $
      ["2", "17", "0", "123"] #-> repl' 0 doubleInt
        -># ["4", "34"]
    it "0 -> Int -> Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> repl' 0 doubleInt
        -># ["4", "Invalid input", "34"]
  describe "replState" do
    it "String -> Int -> (Int, String)" $
      ["2", "5", "12", "0", "11"] #-> replState infAdderStrFunc 0
        -># ["2", "7", "19", "19", "30"]
    it "String -> State Int String" $
      ["2", "5", "12", "0", "11"] #-> replState infAdderStr 0
        -># ["2", "7", "19", "19", "30"]
    it "String -> State Int (Maybe String)" $
      ["2", "5", "12", "0", "11"] #-> replState adderStr 0
        -># ["2", "7", "19"]
    it "String -> State Int (Either String String)" $
      ["2", "5", "12", "0", "11"] #-> replState adderStrBye 0
        -># ["2", "7", "19", "bye"]
    it "Int -> Int -> (Int, Int)" $
      ["2", "5", "12", "0", "11"] #-> replState infAdderFunc 0
        -># ["2", "7", "19", "19", "30"]
    it "Int -> Int -> (Int, Int) (invlid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replState infAdderFunc 0
        -># ["2", "Invalid input", "7", "19", "19", "30"]
    it "Int -> State Int Int" $
      ["2", "5", "12", "0", "11"] #-> replState infAdder 0
        -># ["2", "7", "19", "19", "30"]
    it "Int -> State Int Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replState infAdder 0
        -># ["2", "Invalid input", "7", "19", "19", "30"]
    it "Int -> State Int (Maybe Int)" $
      ["2", "5", "12", "0", "11"] #-> replState adder 0
        -># ["2", "7", "19"]
    it "Int -> State Int (Maybe Int) (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replState adder 0
        -># ["2", "Invalid input", "7", "19"]
    it "Int -> State Int (Either String Int)" $
      ["2", "5", "12", "0", "11"] #-> replState adderBye 0
        -># ["2", "7", "19", "bye"]
    it "Int -> State Int (Either String Int) (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replState adderBye 0
        -># ["2", "Invalid input", "7", "19", "bye"]
  describe "replState'" do
    it "0 -> Int -> State Int Int" $
      ["2", "5", "12", "0", "11"] #-> replState' 0 infAdder 0
        -># ["2", "7", "19"]
    it "0 -> Int -> State Int Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replState' 0 infAdder 0
        -># ["2", "Invalid input", "7", "19"]
  describe "replFold" do
    it "Int -> Int -> Int" $
      ["2", "5", "12", "0", "11"] #-> replFold @Int (+) 0
        -># ["2", "7", "19", "19", "30"]
    it "Int -> Int -> Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replFold @Int (+) 0
        -># ["2", "Invalid input", "7", "19", "19", "30"]
    it "sums of squares" $
      ["2", "5", "12", "0", "11"] #-> replFold @Int (flip ((+) . (^ (2 :: Int)))) 0
        -># ["4", "29", "173", "173", "294"]
  describe "replFold'" do
    it "0 -> Int -> Int -> Int" $
      ["2", "5", "12", "0", "11"] #-> replFold' @Int 0 (+) 0
        -># ["2", "7", "19"]
    it "0 -> Int -> Int -> Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> replFold' @Int 0 (+) 0
        -># ["2", "Invalid input", "7", "19"]
    it "sums of squares (stopped)" $
      ["2", "5", "12", "0", "11"] #-> replFold' @Int 0 (flip ((+) . (^ (2 :: Int)))) 0
        -># ["4", "29", "173"]
  describe "pRepl" do
    it "[String] -> [String]" $
      ["ab", "cdf", "", "efgh"] #-> pRepl ">" (map doubleString)
        ->># [">abab", ">cdfcdf", ">", ">efghefgh"]
    it "String -> String" $
      ["ab", "cdf", "", "efgh"] #-> pRepl ">" doubleString
        ->># [">abab", ">cdfcdf", ">", ">efghefgh"]
    it "String -> Maybe String" $
      ["ab", "cdf", "", "efgh"] #-> pRepl ">" maybeDoubleString
        ->># [">abab", ">cdfcdf"]
    it "String -> Either String String" $
      ["ab", "cdf", "", "efgh"] #-> pRepl ">" eitherDoubleString
        ->># [">abab", ">cdfcdf", ">bye"]
    it "[Int] -> [Int]" $
      ["2", "17", "0", "123"] #-> pRepl ">" (map doubleInt)
        ->># [">4", ">34", ">0", ">246"]
    it "[Int] -> [Int] (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> pRepl ">" (map doubleInt)
        ->># [">4", ">34", ">0", ">246"]
    it "Int -> Int" $
      ["2", "17", "0", "123"] #-> pRepl ">" doubleInt
        ->># [">4", ">34", ">0", ">246"]
    it "Int -> Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> pRepl ">" doubleInt
        ->># [">4", ">Invalid input", ">34", ">0", ">246"]
    it "Int -> Maybe Int" $
      ["2", "17", "0", "123"] #-> pRepl ">" maybeDoubleInt
        ->># [">4", ">34"]
    it "Int -> Maybe Int (invalid input) " $
      ["2", "abc", "17", "0", "123"] #-> pRepl ">" maybeDoubleInt
        ->># [">4", ">Invalid input", ">34"]
    it "Int -> Either String Int" $
      ["2", "17", "0", "123"] #-> pRepl ">" eitherDoubleInt
        ->># [">4", ">34", ">bye"]
    it "Int -> Either String Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> pRepl ">" eitherDoubleInt
        ->># [">4", ">Invalid input", ">34", ">bye"]
  describe "pRepl'" do
    it "0 -> Int -> Int" $
      ["2", "17", "0", "123"] #-> pRepl' ">" 0 doubleInt
        ->># [">4", ">34"]
    it "0 -> Int -> Int (invalid input)" $
      ["2", "abc", "17", "0", "123"] #-> pRepl' ">" 0 doubleInt
        ->># [">4", ">Invalid input", ">34"]
  describe "pReplState" do
    it "String -> Int -> (Int, String)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" infAdderStrFunc 0
        ->># [">2", ">7", ">19", ">19", ">30"]
    it "String -> State Int String" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" infAdderStr 0
        ->># [">2", ">7", ">19", ">19", ">30"]
    it "String -> State Int (Maybe String)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" adderStr 0
        ->># [">2", ">7", ">19"]
    it "String -> State Int (Either String String)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" adderStrBye 0
        ->># [">2", ">7", ">19", ">bye"]
    it "Int -> Int -> (Int, Int)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" infAdderFunc 0
        ->># [">2", ">7", ">19", ">19", ">30"]
    it "Int -> Int -> (Int, Int) (invlid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplState ">" infAdderFunc 0
        ->># [">2", ">Invalid input", ">7", ">19", ">19", ">30"]
    it "Int -> State Int Int" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" infAdder 0
        ->># [">2", ">7", ">19", ">19", ">30"]
    it "Int -> State Int Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplState ">" infAdder 0
        ->># [">2", ">Invalid input", ">7", ">19", ">19", ">30"]
    it "Int -> State Int (Maybe Int)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" adder 0
        ->># [">2", ">7", ">19"]
    it "Int -> State Int (Maybe Int) (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplState ">" adder 0
        ->># [">2", ">Invalid input", ">7", ">19"]
    it "Int -> State Int (Either String Int)" $
      ["2", "5", "12", "0", "11"] #-> pReplState ">" adderBye 0
        ->># [">2", ">7", ">19", ">bye"]
    it "Int -> State Int (Either String Int) (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplState ">" adderBye 0
        ->># [">2", ">Invalid input", ">7", ">19", ">bye"]
  describe "pReplState'" do
    it "0 -> Int -> State Int Int" $
      ["2", "5", "12", "0", "11"] #-> pReplState' ">" 0 infAdder 0
        ->># [">2", ">7", ">19"]
    it "0 -> Int -> State Int Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplState' ">" 0 infAdder 0
        ->># [">2", ">Invalid input", ">7", ">19"]
  describe "pReplFold" do
    it "Int -> Int -> Int" $
      ["2", "5", "12", "0", "11"] #-> pReplFold @Int ">" (+) 0
        ->># [">2", ">7", ">19", ">19", ">30"]
    it "Int -> Int -> Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplFold @Int ">" (+) 0
        ->># [">2", ">Invalid input", ">7", ">19", ">19", ">30"]
    it "sums of squares" $
      ["2", "5", "12", "0", "11"] #-> pReplFold @Int ">" (flip ((+) . (^ (2 :: Int)))) 0
        ->># [">4", ">29", ">173", ">173", ">294"]
  describe "pReplFold'" do
    it "0 -> Int -> Int -> Int" $
      ["2", "5", "12", "0", "11"] #-> pReplFold' @Int ">" 0 (+) 0
        ->># [">2", ">7", ">19"]
    it "0 -> Int -> Int -> Int (invalid input)" $
      ["2", "abc", "5", "12", "0", "11"] #-> pReplFold' @Int ">" 0 (+) 0
        ->># [">2", ">Invalid input", ">7", ">19"]
    it "sums of squares (stopped)" $
      ["2", "5", "12", "0", "11"] #-> pReplFold' @Int ">" 0 (flip ((+) . (^ (2 :: Int)))) 0
        ->># [">4", ">29", ">173"]

(#->) :: [String] -> IO () -> IO ()
(#->) = withStdin . pack . unlines

(->#) :: IO () -> [String] -> Expectation
testIO -># expected = capture_ testIO `shouldReturn` unlines expected

(->>#) :: IO () -> [String] -> Expectation
testIO ->># expected = capture_ testIO `shouldReturn` ((++ ">") . unlines) expected

doubleString :: String -> String
doubleString s = s ++ s

maybeDoubleString :: String -> Maybe String
maybeDoubleString s = if null s then Nothing else Just $ s ++ s

eitherDoubleString :: String -> Either String String
eitherDoubleString s = if null s then Left "bye" else Right $ s ++ s

doubleInt :: Int -> Int
doubleInt x = x + x

maybeDoubleInt :: Int -> Maybe Int
maybeDoubleInt x = if x == 0 then Nothing else Just $ x + x

eitherDoubleInt :: Int -> Either String Int
eitherDoubleInt x = if x == 0 then Left "bye" else Right $ x + x

infAdderStrFunc :: String -> Int -> (String, Int)
infAdderStrFunc s y = let y' = y + read s in (show y', y')

infAdderStr :: String -> State Int String
infAdderStr s = modify (+ read s) >> gets show

adderStr :: String -> State Int (Maybe String)
adderStr s = case read s of
  0 -> return Nothing
  x -> modify (+ x) >> gets (Just . show)

adderStrBye :: String -> State Int (Either String String)
adderStrBye s = case read s of
  0 -> return $ Left "bye"
  x -> modify (+ x) >> gets (Right . show)

infAdderFunc :: Int -> Int -> (Int, Int)
infAdderFunc x y = let y' = y + x in (y', y')

infAdder :: Int -> State Int Int
infAdder x = modify (+ x) >> get

adder :: Int -> State Int (Maybe Int)
adder x
  | x == 0 = return Nothing
  | otherwise = modify (+ x) >> gets Just

adderBye :: Int -> State Int (Either String Int)
adderBye x
  | x == 0 = return $ Left "bye"
  | otherwise = modify (+ x) >> gets Right
