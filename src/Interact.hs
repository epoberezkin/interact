{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Interact where

import Control.Monad.State
import Data.Either
import Data.Maybe

class Repl a b where
  repl :: (a -> b) -> IO ()

instance {-# OVERLAPPING #-} Repl [String] [String] where
  repl :: ([String] -> [String]) -> IO ()
  repl f = interact $ unlines . f . lines

instance (Read a, Show b) => Repl a b where
  repl f = repl (show . f . read)

instance {-# OVERLAPPING #-} Repl String String where
  repl :: (String -> String) -> IO ()
  repl f = repl $ map f

instance {-# OVERLAPPING #-} Repl String (Maybe String) where
  repl :: (String -> Maybe String) -> IO ()
  repl f = repl $ whileJust . map f

whileJust :: [Maybe String] -> [String]
whileJust = map fromJust . takeWhile isJust

instance {-# OVERLAPPING #-} Repl String (Either String String) where
  repl :: (String -> Either String String) -> IO ()
  repl f = repl $ whileRight . map f

whileRight :: [Either String String] -> [String]
whileRight = rights . rightsAndLeft . span isRight
  where
    rightsAndLeft ([], (Left l : _)) = [Right l]
    rightsAndLeft ([], _) = []
    rightsAndLeft (r : rs, ls) = r : rightsAndLeft (rs, ls)

instance {-# OVERLAPPING #-} (Read a, Show b) => Repl a (Maybe b) where
  repl :: (a -> Maybe b) -> IO ()
  repl f = repl (fmap show . f . read)

instance {-# OVERLAPPING #-} (Read a, Show b) => Repl a (Either String b) where
  repl :: (a -> Either String b) -> IO ()
  repl f = repl (fmap show . f . read)

repl' :: (Eq a, Read a, Show b) => a -> (a -> b) -> IO ()
repl' stop f = repl f'
  where
    f' :: String -> Maybe String
    f' (read -> x)
      | x == stop = Nothing
      | otherwise = Just . show $ f x

class ReplState a b s | b -> s where
  replState :: (a -> b) -> s -> IO ()

instance {-# OVERLAPPING #-} ReplState String (s -> (s, String)) s where
  replState :: (String -> s -> (s, String)) -> s -> IO ()
  replState f s0 = repl $ g s0
    where
      g _ [] = []
      g s (x : xs) = let (s', x') = f x s in x' : g s' xs

instance (Read a, Show b) => ReplState a (s -> (s, b)) s where
  replState :: (a -> s -> (s, b)) -> s -> IO ()
  replState f = replState f'
    where
      f' s st =
        let (st', x) = f (read s) st
         in (st', show x)

instance {-# OVERLAPPING #-} ReplState [String] (State s [String]) s where
  replState :: ([String] -> State s [String]) -> s -> IO ()
  replState f s0 = interact linesWithState
    where
      linesWithState str = unlines $ evalState (f $ lines str) s0

instance (Read a, Show b) => ReplState a (State s b) s where
  replState :: (a -> State s b) -> s -> IO ()
  replState f = replState $ fmap show . f . read

instance {-# OVERLAPPING #-} ReplState String (State s String) s where
  replState :: (String -> State s String) -> s -> IO ()
  replState f = replState @[String] $ mapM f

instance {-# OVERLAPPING #-} ReplState String (State s (Maybe String)) s where
  replState :: (String -> State s (Maybe String)) -> s -> IO ()
  replState f = replState $ fmap whileJust . mapM f

instance {-# OVERLAPPING #-} ReplState String (State s (Either String String)) s where
  replState :: (String -> State s (Either String String)) -> s -> IO ()
  replState f = replState $ fmap whileRight . mapM f

instance {-# OVERLAPPING #-} (Read a, Show b) => ReplState a (State s (Maybe b)) s where
  replState :: (a -> State s (Maybe b)) -> s -> IO ()
  replState f = replState $ fmap (fmap show) . f . read

instance {-# OVERLAPPING #-} (Read a, Show b) => ReplState a (State s (Either String b)) s where
  replState :: (a -> State s (Either String b)) -> s -> IO ()
  replState f = replState $ fmap (fmap show) . f . read

replState' :: forall a b s. (Eq a, Read a, Show b) => a -> (a -> State s b) -> s -> IO ()
replState' stop f = replState f'
  where
    f' :: String -> State s (Maybe String)
    f' (read -> x)
      | x == stop = pure Nothing
      | otherwise = Just . show <$> f x

replFold :: forall a b. (Read a, Show b) => (b -> a -> b) -> b -> IO ()
replFold f = replState f'
  where
    f' :: String -> State b String
    f' s = modify (`f` read s) >> gets show

replFold' :: forall a b. (Eq a, Read a, Show b) => a -> (b -> a -> b) -> b -> IO ()
replFold' stop f = replState' stop f'
  where
    f' :: a -> State b b
    f' x = modify (`f` x) >> get
