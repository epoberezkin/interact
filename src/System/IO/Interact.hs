{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : System.IO.Interact
-- Copyright   : (c) Evgeny Poberezkin
-- License     : MIT
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- This module provides functions to instantly create interactive REPL,
-- similar to Prelude 'interact' but with line-by-line processing:
--
-- - stateless REPL from a single argument functions
-- - REPL with state from plain state function or with State monad
-- - REPL-fold from two-arguments functions, with the accumulator in the first argument
--
-- Each line you enter is 'read' into the argument type and sent to the function, with the result printed.
module System.IO.Interact
  ( -- * Stateless REPL
    Repl,
    repl,
    repl',
    pRepl,
    pRepl',

    -- * REPL with state
    ReplState,
    replState,
    replState',
    pReplState,
    pReplState',

    -- * REPL-fold
    replFold,
    replFold',
    pReplFold,
    pReplFold',
  )
where

import Control.Exception (bracket)
import Control.Monad.State
import Data.Maybe
import System.IO
import Text.Read (readMaybe)

-- | 'Repl' typeclass with polymorphic stateless function 'repl' to interactively
-- evaluate input lines and print responses (see below).
class Repl a b where
  -- | Function passed to 'repl' will be called with values from 'stdin'
  -- ('String's or 'Read' instances, one value at a time or as a lazy list
  -- depending on the type of the function) and should return value
  -- to be printed to 'stdout' ('String' or 'Show' instance, possibly
  -- wrapped in 'Maybe' or 'Either', one value at a time or as a lazy list) .
  --
  -- Specific behaviour depends on function type (see instances above).
  --
  -- __Examples__:
  --
  -- Print square roots of the entered numbers:
  --
  -- > repl (sqrt :: Double -> Double)
  --
  -- Reverse entered strings:
  --
  -- > repl (reverse :: String -> String)
  --
  -- Prints both squares and square roots:
  --
  -- > sqrSqrt :: [Double] -> [Double]
  -- > sqrSqrt [] = []
  -- > sqrSqrt (x:xs) = x^2 : sqrt x : sqrSqrt xs
  -- > repl sqrSqrt
  repl :: (a -> b) -> IO ()
  repl = pRepl ""

  -- | 'pRepl' is 'repl' with prompt
  --
  -- __Example__:
  --
  -- > pRepl ">" (sqrt :: Double -> Double)
  pRepl :: String -> (a -> b) -> IO ()

-- | 'stdin'/'stdout' 'String's as lazy lists
instance {-# OVERLAPPING #-} Repl [String] [String] where
  pRepl :: String -> ([String] -> [String]) -> IO ()
  pRepl "" f = interact $ unlines . f . lines
  pRepl p f =
    noBuffering . interact $
      (p ++) . concatMap (++ '\n' : p) . f . lines

noBuffering :: IO a -> IO a
noBuffering = withBufferMode NoBuffering stdout

withBufferMode :: BufferMode -> Handle -> IO a -> IO a
withBufferMode mode h act =
  bracket
    (hGetBuffering h <* hSetBuffering h mode)
    (hSetBuffering h)
    (const act)

-- | 'stdin'/'stdout' values as lazy lists
instance {-# OVERLAPPING #-} (Read a, Show b) => Repl [a] [b] where
  pRepl :: String -> ([a] -> [b]) -> IO ()
  pRepl p f = pRepl p $ map show . f . mapMaybe readMaybe

-- | Ctrl-D to exit
instance (Read a, Show b) => Repl a b where
  pRepl :: String -> (a -> b) -> IO ()
  pRepl p f = pRepl p $ maybe invalid show . fmap f . readMaybe

invalid :: String
invalid = "Invalid input"

-- | 'String's do not use 'read'/'show'
instance {-# OVERLAPPING #-} Repl String String where
  pRepl :: String -> (String -> String) -> IO ()
  pRepl p f = pRepl p $ map f

instance {-# OVERLAPPING #-} Repl String (Maybe String) where
  pRepl :: String -> (String -> Maybe String) -> IO ()
  pRepl p f = pRepl p $ whileJust . map f

whileJust :: [Maybe String] -> [String]
whileJust = map fromJust . takeWhile isJust

instance {-# OVERLAPPING #-} Repl String (Either String String) where
  pRepl :: String -> (String -> Either String String) -> IO ()
  pRepl p f = pRepl p $ whileRight . map f

whileRight :: [Either String String] -> [String]
whileRight (Right x : xs) = x : whileRight xs
whileRight (Left x : _) = [x]
whileRight [] = []

-- | return 'Nothing' to exit
instance {-# OVERLAPPING #-} (Read a, Show b) => Repl a (Maybe b) where
  pRepl :: String -> (a -> Maybe b) -> IO ()
  pRepl p f = pRepl p $ readShow f

-- | return 'Left' to exit, string in 'Left' is printed
instance {-# OVERLAPPING #-} (Read a, Show b) => Repl a (Either String b) where
  pRepl :: String -> (a -> Either String b) -> IO ()
  pRepl p f = pRepl p $ readShow f

readShow :: (Applicative f, Read a, Show b) => (a -> f b) -> String -> f String
readShow f = maybe (pure invalid) (fmap show) . fmap f . readMaybe

-- | Same as 'repl' with @(a -> b)@ function but the first argument is
-- the value that will cause 'repl'' to exit.
repl' :: (Eq a, Read a, Show b) => a -> (a -> b) -> IO ()
repl' = pRepl' ""

pRepl' ::
  forall a b.
  (Eq a, Read a, Show b) =>
  -- | prompt
  String ->
  -- | value to stop
  a ->
  -- | function to transform the input
  (a -> b) ->
  IO ()
pRepl' p stop f = pRepl p $ readShow f'
  where
    f' :: a -> Maybe b
    f' x
      | x == stop = Nothing
      | otherwise = Just $ f x

-- | 'ReplState' typeclass with polymorphic stateful function 'replState'
-- to interactively evaluate input lines and print responses (see below).
class ReplState a b s | b -> s where
  -- | Function passed to 'replState' will be called with values from 'stdin'
  -- and previous state (depending on type, via State monad or
  -- as the first argument) and should return value to be printed to 'stdout'
  -- and the new state (either via State monad or as a tuple).
  --
  -- Specific behaviour depends on function type (see instances above).
  --
  -- __Examples__:
  --
  -- Prints sums of entered numbers:
  --
  -- > adder :: Int -> State Int Int
  -- > adder x = modify (+ x) >> get
  -- > replState adder 0
  --
  -- or with plain state function
  --
  -- > adder :: Int -> Int -> (Int, Int)
  -- > adder x s = let s' = s + x in (s', s')
  -- > replState adder 0
  --
  -- Above can be done with 'replFold' (see below):
  --
  -- > replFold (+) 0
  --
  -- but replState is more flexible - state and output can be different types.
  replState ::
    -- | state function (type defined by the instances)
    (a -> b) ->
    -- | initial state
    s ->
    IO ()
  replState = pReplState ""

  -- | 'replState' with prompt defined by the first argument
  pReplState :: String -> (a -> b) -> s -> IO ()

-- | plain state function with 'String's as argument and result
instance {-# OVERLAPPING #-} ReplState String (s -> (String, s)) s where
  pReplState :: String -> (String -> s -> (String, s)) -> s -> IO ()
  pReplState p f = pReplState p f'
    where
      f' :: String -> State s String
      f' = state . f

-- | plain state function with argument and result of any 'Read'/'Show' types
instance (Read a, Show b) => ReplState a (s -> (b, s)) s where
  pReplState :: String -> (a -> s -> (b, s)) -> s -> IO ()
  pReplState p f = pReplState p f'
    where
      f' :: a -> State s b
      f' = state . f

-- | 'stdin'/'stdout' 'String's as lazy lists
instance {-# OVERLAPPING #-} ReplState [String] (State s [String]) s where
  pReplState :: String -> ([String] -> State s [String]) -> s -> IO ()
  pReplState p f s0 = pRepl p $ (`evalState` s0) . f

-- | Ctrl-D to exit
instance (Read a, Show b) => ReplState a (State s b) s where
  pReplState :: String -> (a -> State s b) -> s -> IO ()
  pReplState p f = pReplState p $ readShow f

-- | 'String's do not use 'read'/'show'
instance {-# OVERLAPPING #-} ReplState String (State s String) s where
  pReplState :: String -> (String -> State s String) -> s -> IO ()
  pReplState p f = pReplState @[String] p $ mapM f

instance {-# OVERLAPPING #-} ReplState String (State s (Maybe String)) s where
  pReplState ::
    String -> (String -> State s (Maybe String)) -> s -> IO ()
  pReplState p f = pReplState p $ fmap whileJust . mapM f

instance {-# OVERLAPPING #-} ReplState String (State s (Either String String)) s where
  pReplState ::
    String -> (String -> State s (Either String String)) -> s -> IO ()
  pReplState p f = pReplState p $ fmap whileRight . mapM f

-- | return 'Nothing' to exit
instance {-# OVERLAPPING #-} (Read a, Show b) => ReplState a (State s (Maybe b)) s where
  pReplState :: String -> (a -> State s (Maybe b)) -> s -> IO ()
  pReplState p f = pReplState p $ readShow' f

-- | return 'Left' to exit, string in 'Left' is printed
instance {-# OVERLAPPING #-} (Read a, Show b) => ReplState a (State s (Either String b)) s where
  pReplState :: String -> (a -> State s (Either String b)) -> s -> IO ()
  pReplState p f = pReplState p $ readShow' f

readShow' ::
  (Applicative f, Read a, Show b) => (a -> State s (f b)) -> String -> State s (f String)
readShow' f = maybe (pure $ pure invalid) (fmap $ fmap show) . fmap f . readMaybe

-- | Same as 'replState' with @(a -> State s b)@ function but the first
-- argument is the value that will cause 'replState'' to exit.
replState' ::
  (Eq a, Read a, Show b) => a -> (a -> State s b) -> s -> IO ()
replState' = pReplState' ""

-- | 'replState'' with prompt
pReplState' ::
  forall a b s.
  (Eq a, Read a, Show b) =>
  -- | prompt
  String ->
  -- | value to stop
  a ->
  -- | state function
  (a -> State s b) ->
  -- | initial state
  s ->
  IO ()
pReplState' p stop f = pReplState p $ readShow' f'
  where
    f' :: a -> State s (Maybe b)
    f' x
      | x == stop = pure Nothing
      | otherwise = Just <$> f x

-- f' s = case readMaybe s of
--   Nothing -> pure $ pure invalid
--   Just x
--     | x == stop -> pure Nothing
--     | otherwise -> fmap pure . fmap show $ f x

-- | 'replFold' combines the entered values with the accumulated value using
-- provided function and prints the resulting values.
replFold ::
  (Read a, Show b) => (b -> a -> b) -> b -> IO ()
replFold = pReplFold ""

-- | 'replFold' with prompt
pReplFold :: (Read a, Show b) => String -> (b -> a -> b) -> b -> IO ()
pReplFold p f = pReplState p . readShow $ foldState f

foldState :: (b -> a -> b) -> a -> State b b
foldState f x = modify (`f` x) >> get

-- | Same as 'replFold' but the first argument is the value that will cause
-- 'replFold'' to exit.
replFold' ::
  (Eq a, Read a, Show b) => a -> (b -> a -> b) -> b -> IO ()
replFold' = pReplFold' ""

-- | 'replFold'' with prompt
pReplFold' ::
  (Eq a, Read a, Show b) =>
  -- | prompt
  String ->
  -- | value to stop
  a ->
  -- | folding function
  (b -> a -> b) ->
  -- | initial value
  b ->
  IO ()
pReplFold' p stop f = pReplState' p stop $ foldState f
