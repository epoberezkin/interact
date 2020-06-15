module Main where

import Control.Monad.State
import Interact

main :: IO ()
main = replFunc double

-- main = replState adder 0

-- main = repl process

process :: String -> Maybe String
process s = if length s > 3 then Just s else Nothing

double :: Int -> Either String Int
double x = if x == 0 then Left "bye" else Right $ 2 * x

adder :: String -> State Int (Either String String)
adder s = do
  let y = read s
  if y == 0
    then return $ Left "bye bye"
    else do
      modify (+ y)
      x <- get
      return . Right $ show x
