{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Interact where

import Control.Monad.State
import Data.Either
import Data.Maybe

class Repl from to where
  repl :: (from -> to) -> IO ()

instance Repl [String] [String] where
  repl :: ([String] -> [String]) -> IO ()
  repl f = interact $ unlines . f . lines

instance Repl String String where
  repl :: (String -> String) -> IO ()
  repl f = repl $ map f

instance Repl String (Maybe String) where
  repl :: (String -> Maybe String) -> IO ()
  repl f = repl $ whileJust . map f

whileJust :: [Maybe String] -> [String]
whileJust = map fromJust . takeWhile isJust

instance Repl String (Either String String) where
  repl :: (String -> Either String String) -> IO ()
  repl f = repl $ rightsAndLeft . map f

rightsAndLeft :: [Either String String] -> [String]
rightsAndLeft = rights . rnl . span isRight
  where
    rnl ([], (Left l : _)) = [Right l]
    rnl ([], _) = []
    rnl (r : rs, ls) = r : rnl (rs, ls)

class (Read from, Show to) => ReplFunc from to where
  replFunc :: (from -> to) -> IO ()
  replFunc f = repl (show . f . read)

instance (Read from, Show to) => ReplFunc from (Maybe to) where
  replFunc :: (from -> Maybe to) -> IO ()
  replFunc f = repl (fmap show . f . read)

instance (Read from, Show to) => ReplFunc from (Either String to) where
  replFunc :: (from -> Either String to) -> IO ()
  replFunc f = repl (fmap show . f . read)

class ReplState from to s | to -> s where
  replState :: (from -> to) -> s -> IO ()

instance ReplState String (s -> (s, String)) s where
  replState :: (String -> s -> (s, String)) -> s -> IO ()
  replState f s0 = repl $ g s0
    where
      g _ [] = []
      g s (x : xs) = let (s', x') = f x s in x' : g s' xs

instance ReplState [String] (State s [String]) s where
  replState :: ([String] -> State s [String]) -> s -> IO ()
  replState f s0 = interact linesWithState
    where
      linesWithState str = unlines $ evalState (f $ lines str) s0

instance ReplState String (State s String) s where
  replState :: (String -> State s String) -> s -> IO ()
  replState f = replState @[String] $ mapM f

instance ReplState String (State s (Maybe String)) s where
  replState :: (String -> State s (Maybe String)) -> s -> IO ()
  replState f = replState $ fmap whileJust . mapM f

instance ReplState String (State s (Either String String)) s where
  replState :: (String -> State s (Either String String)) -> s -> IO ()
  replState f = replState $ fmap rightsAndLeft . mapM f
