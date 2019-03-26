module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, modify, get, put)
import Data.List (isPrefixOf)
import BraunHeap
  (SomeHeap(..), Heap(Empty), insertSome, popSome, sizeSome, prettyShow)
import System.Console.Repline
  (HaskelineT, WordCompleter, CompleterStyle(Word), evalRepl)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)

main :: IO ()
main =
  flip evalStateT (SomeHeap Empty) $
  evalRepl
    ">>> "
    cmd
    [("help", const help), ("quit", const $ liftIO exitSuccess)]
    (Word completer)
    (liftIO . putStrLn . unlines $
     [ "Braun Heap"
     , ""
     , "A simple CLI for manipulating an integer Braun heap with type-level constraints on subtrees"
     , ""
     , "Available commands: help, push, pop, size, print"
     ])

cmd :: String -> HaskelineT (StateT (SomeHeap Int) IO) ()
cmd input
  | "help" == input = help
  | "print" == input = get >>= liftIO . putStrLn . prettyShow
  | "size" == input = get >>= liftIO . print . sizeSome
  | "pop" == input = do
      s <- get
      case popSome s of
        Nothing -> liftIO $ putStrLn "Heap is already empty"
        Just (n, heap) -> do
          put heap
          liftIO . putStrLn $ show n <> "\nNew size: " <> show (sizeSome heap)
  | "push " `isPrefixOf` input =
      case readMaybe (drop 5 input) of
        Nothing -> liftIO $ putStrLn ("Invalid input: " <> drop 5 input)
        Just n -> do
          modify (insertSome n)
          get >>= \h -> liftIO . putStrLn $ "New size: " <> show (sizeSome h)
  | otherwise =
      liftIO $ putStrLn ("Invalid input: " <> input)

completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) names
  where
    names = ["push", "pop", "size", "print", "help"]

help :: HaskelineT (StateT (SomeHeap Int) IO) ()
help = liftIO . putStrLn . unlines $
  [ "Available commands:"
  , ""
  , "print  - pretty-print current state of the heap"
  , "size   - print current size of the heap"
  , "push N - insert a single number to the heap"
  , "pop    - pop a single number from the heap"
  , ":quit  - Quit the REPL"
  , ":help  - This text"
  ]
