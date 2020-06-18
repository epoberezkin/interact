{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Interact
-- Copyright   : (c) Evgeny Poberezkin
-- License     : MIT
--
-- Maintainer  : evgeny@poberezkin.com
-- Stability   : experimental
-- Portability : non-portable
--
-- DIY REPL: Prelude's 'interact' on steroids
--
-- Functions to create interactive REPLs.
module Interact
  ( -- * Stateless REPLs
    Interact,
    repl,
    repl',

    -- * Stateful REPLs
    InteractState,
    replState,
    replState',

    -- * REPL folds
    replFold,
    replFold',
  )
where

import Control.Monad.State
import Data.Either
import Data.Maybe

-- | 'Interact' typeclass with polymorphic stateless function 'repl' to interactively
-- evaluate input lines and print responses (see below).
class Interact a b where
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
  repl :: (a -> b) -> IO ()

-- | 'stdin'/'stdout' 'String's as lazy lists
instance {-# OVERLAPPING #-} Interact [String] [String] where
  repl :: ([String] -> [String]) -> IO ()
  repl f = interact $ unlines . f . lines

-- | 'stdin'/'stdout' values as lazy lists
instance {-# OVERLAPPING #-} (Read a, Show b) => Interact [a] [b] where
  repl :: ([a] -> [b]) -> IO ()
  repl f = repl $ map show . f . map read

-- | Ctrl-D to exit
instance (Read a, Show b) => Interact a b where
  repl :: (a -> b) -> IO ()
  repl f = repl (show . f . read)

-- | 'String's do not use 'read'/'show'
instance {-# OVERLAPPING #-} Interact String String where
  repl :: (String -> String) -> IO ()
  repl f = repl $ map f

instance {-# OVERLAPPING #-} Interact String (Maybe String) where
  repl :: (String -> Maybe String) -> IO ()
  repl f = repl $ whileJust . map f

whileJust :: [Maybe String] -> [String]
whileJust = map fromJust . takeWhile isJust

instance {-# OVERLAPPING #-} Interact String (Either String String) where
  repl :: (String -> Either String String) -> IO ()
  repl f = repl $ whileRight . map f

whileRight :: [Either String String] -> [String]
whileRight = rights . rightsAndLeft . span isRight
  where
    rightsAndLeft ([], (Left l : _)) = [Right l]
    rightsAndLeft ([], _) = []
    rightsAndLeft (r : rs, ls) = r : rightsAndLeft (rs, ls)

-- | return 'Nothing' to exit
instance {-# OVERLAPPING #-} (Read a, Show b) => Interact a (Maybe b) where
  repl :: (a -> Maybe b) -> IO ()
  repl f = repl (fmap show . f . read)

-- | return 'Left' to exit, string in 'Left' is printed
instance {-# OVERLAPPING #-} (Read a, Show b) => Interact a (Either String b) where
  repl :: (a -> Either String b) -> IO ()
  repl f = repl (fmap show . f . read)

-- | Same as 'repl' with @(a -> b)@ function but the first argument is
-- the value that will cause 'repl'' to exit.
repl' :: (Eq a, Read a, Show b) => a -> (a -> b) -> IO ()
repl' stop f = repl f'
  where
    f' :: String -> Maybe String
    f' (read -> x)
      | x == stop = Nothing
      | otherwise = Just . show $ f x

-- | 'InteractState' typeclass with polymorphic stateful function 'replState'
-- to interactively evaluate input lines and print responses (see below).
class InteractState a b s | b -> s where
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
  replState :: (a -> b) -> s -> IO ()

-- | plain state function with 'String's as argument and result
instance {-# OVERLAPPING #-} InteractState String (s -> (String, s)) s where
  replState :: (String -> s -> (String, s)) -> s -> IO ()
  replState f s0 = repl $ g s0
    where
      g _ [] = []
      g s (x : xs) = let (x', s') = f x s in x' : g s' xs

-- | plain state function with argument and result of any 'Read'/'Show' types
instance (Read a, Show b) => InteractState a (s -> (b, s)) s where
  replState :: (a -> s -> (b, s)) -> s -> IO ()
  replState f = replState f'
    where
      f' s st =
        let (x, st') = f (read s) st
         in (show x, st')

-- | 'stdin'/'stdout' 'String's as lazy lists
instance {-# OVERLAPPING #-} InteractState [String] (State s [String]) s where
  replState :: ([String] -> State s [String]) -> s -> IO ()
  replState f s0 = interact linesWithState
    where
      linesWithState str = unlines $ evalState (f $ lines str) s0

-- | Ctrl-D to exit
instance (Read a, Show b) => InteractState a (State s b) s where
  replState :: (a -> State s b) -> s -> IO ()
  replState f = replState $ fmap show . f . read

-- | 'String's do not use 'read'/'show'
instance {-# OVERLAPPING #-} InteractState String (State s String) s where
  replState :: (String -> State s String) -> s -> IO ()
  replState f = replState @[String] $ mapM f

instance {-# OVERLAPPING #-} InteractState String (State s (Maybe String)) s where
  replState :: (String -> State s (Maybe String)) -> s -> IO ()
  replState f = replState $ fmap whileJust . mapM f

instance {-# OVERLAPPING #-} InteractState String (State s (Either String String)) s where
  replState :: (String -> State s (Either String String)) -> s -> IO ()
  replState f = replState $ fmap whileRight . mapM f

-- | return 'Nothing' to exit
instance {-# OVERLAPPING #-} (Read a, Show b) => InteractState a (State s (Maybe b)) s where
  replState :: (a -> State s (Maybe b)) -> s -> IO ()
  replState f = replState $ fmap (fmap show) . f . read

-- | return 'Left' to exit, string in 'Left' is printed
instance {-# OVERLAPPING #-} (Read a, Show b) => InteractState a (State s (Either String b)) s where
  replState :: (a -> State s (Either String b)) -> s -> IO ()
  replState f = replState $ fmap (fmap show) . f . read

-- | Same as 'replState' with @(a -> State s b)@ function but the first
-- argument is the value that will cause 'replState'' to exit.
replState' ::
  forall a b s. (Eq a, Read a, Show b) => a -> (a -> State s b) -> s -> IO ()
replState' stop f = replState f'
  where
    f' :: String -> State s (Maybe String)
    f' (read -> x)
      | x == stop = pure Nothing
      | otherwise = Just . show <$> f x

-- | 'replFold' combines the entered values with the accumulated value using
-- provided function and prints the resulting values.
replFold ::
  forall a b. (Read a, Show b) => (b -> a -> b) -> b -> IO ()
replFold f = replState f'
  where
    f' :: String -> b -> (String, b)
    f' (read -> x) y = let y' = f y x in (show y', y')

-- | Same as 'replFold' but the first argument is the value that will cause
-- 'replFold'' to exit.
replFold' ::
  forall a b. (Eq a, Read a, Show b) => a -> (b -> a -> b) -> b -> IO ()
replFold' stop f = replState' stop f'
  where
    f' :: a -> State b b
    f' x = modify (`f` x) >> get
