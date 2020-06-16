{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import Interact
import System.IO.Silently (capture_)
import Test.Hspec
import Test.Main (withStdin)

main :: IO ()
main = hspec do
  describe "repl" do
    it "[String] -> [String]" $
      inputLines @-> repl (map doubleString) -># doubledLines
    it "String -> String" $
      inputLines @-> repl doubleString -># doubledLines
    it "String -> Maybe String" $
      inputLines @-> repl maybeDoubleString -># doubledLinesStopped
    it "String -> Either String String" $
      inputLines @-> repl eitherDoubleString -># doubledLinesBye
    it "[Int] -> [Int]" $
      inputInts @-> repl (map doubleInt) -># doubledInts
    it "Int -> Int" $
      inputInts @-> repl doubleInt -># doubledInts
    it "Int -> Maybe Int" $
      inputInts @-> repl maybeDoubleInt -># doubledIntsStopped
    it "Int -> Either String Int" $
      inputInts @-> repl eitherDoubleInt -># doubledIntsBye
  describe "repl'" do
    it "0 -> Int -> Int" $
      inputInts @-> repl' 0 doubleInt -># doubledIntsStopped
  describe "replState" do
    it "String -> Int -> (Int, String)" $
      intsToAdd @-> replState infAdderStrFunc 0 -># addedInts
    it "String -> State Int String" $
      intsToAdd @-> replState infAdderStr 0 -># addedInts
    it "String -> State Int (Maybe String)" $
      intsToAdd @-> replState adderStr 0 -># addedIntsStopped
    it "String -> State Int (Either String String)" $
      intsToAdd @-> replState adderStrBye 0 -># addedIntsBye
    it "Int -> Int -> (Int, Int)" $
      intsToAdd @-> replState infAdderFunc 0 -># addedInts
    it "Int -> State Int Int" $
      intsToAdd @-> replState infAdder 0 -># addedInts
    it "Int -> State Int (Maybe Int)" $
      intsToAdd @-> replState adder 0 -># addedIntsStopped
    it "Int -> State Int (Either String Int)" $
      intsToAdd @-> replState adderBye 0 -># addedIntsBye
  describe "replState'" do
    it "0 -> Int -> State Int Int" $
      intsToAdd @-> replState' 0 infAdder 0 -># addedIntsStopped
  describe "replFold" do
    it "Int -> Int -> Int" $
      intsToAdd @-> replFold @Int (+) 0
        -># addedInts
    it "sums of squares" $
      intsToAdd @-> replFold @Int (flip ((+) . (^ (2 :: Int)))) 0
        -># sumsOfSquares
  describe "replFold'" do
    it "0 -> Int -> Int -> Int" $
      intsToAdd @-> replFold' @Int 0 (+) 0
        -># addedIntsStopped
    it "sums of squares (stopped)" $
      intsToAdd @-> replFold' @Int 0 (flip ((+) . (^ (2 :: Int)))) 0
        -># sumsOfSquaresStopped

(@->) :: ByteString -> IO () -> IO ()
(@->) = withStdin

(->#) :: IO () -> String -> Expectation
testedIO -># expected = capture_ testedIO `shouldReturn` expected

-- samples for repl with String

doubleString :: String -> String
doubleString s = s ++ s

maybeDoubleString :: String -> Maybe String
maybeDoubleString s = if null s then Nothing else Just $ s ++ s

eitherDoubleString :: String -> Either String String
eitherDoubleString s = if null s then Left "bye" else Right $ s ++ s

inputLines :: ByteString
inputLines = "ab\ncdf\n\nefgh\n"

doubledLines :: String
doubledLines = "abab\ncdfcdf\n\nefghefgh\n"

doubledLinesStopped :: String
doubledLinesStopped = "abab\ncdfcdf\n"

doubledLinesBye :: String
doubledLinesBye = "abab\ncdfcdf\nbye\n"

-- samples for repl with Read/Show instances

doubleInt :: Int -> Int
doubleInt x = x + x

maybeDoubleInt :: Int -> Maybe Int
maybeDoubleInt x = if x == 0 then Nothing else Just $ x + x

eitherDoubleInt :: Int -> Either String Int
eitherDoubleInt x = if x == 0 then Left "bye" else Right $ x + x

inputInts :: ByteString
inputInts = "2\n17\n0\n123\n"

doubledInts :: String
doubledInts = "4\n34\n0\n246\n"

doubledIntsStopped :: String
doubledIntsStopped = "4\n34\n"

doubledIntsBye :: String
doubledIntsBye = "4\n34\nbye\n"

-- samples for replState with String

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

intsToAdd :: ByteString
intsToAdd = "2\n5\n12\n0\n11\n"

addedInts :: String
addedInts = "2\n7\n19\n19\n30\n"

sumsOfSquares :: String
sumsOfSquares = "4\n29\n173\n173\n294\n"

addedIntsStopped :: String
addedIntsStopped = "2\n7\n19\n"

sumsOfSquaresStopped :: String
sumsOfSquaresStopped = "4\n29\n173\n"

addedIntsBye :: String
addedIntsBye = "2\n7\n19\nbye\n"
