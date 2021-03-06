module Main
  ( main
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify, put)
import Data.List (isPrefixOf)
import BraunHeap.TypeLits
  ( Heap(Empty)
  , SomeHeap(..)
  , addSome
  , popSome
  , prettyShow
  , sizeSome
  )
import System.Console.Repline
  ( CompleterStyle(Word)
  , HaskelineT
  , WordCompleter
  , evalRepl
  )
import System.Exit (exitSuccess)
import Text.Read (readMaybe)


type Repl a = HaskelineT (StateT (SomeHeap Int) IO) a

main :: IO ()
main =
  evalStateT
    (evalRepl ">>> " cmd colonCmd (Word completer) banner)
    (SomeHeap Empty)

cmd :: String -> Repl ()
cmd input = case filter (not . null) (words input) of
  "add":xs -> mapM_ add xs
  ["pop"] -> pop
  ["print"] -> liftIO . putStrLn . prettyShow =<< get
  ["size"] -> liftIO . print . sizeSome =<< get
  ["help"] -> help
  _ -> liftIO $ putStrLn ("Invalid input: " <> input)

pop :: Repl ()
pop = do
  s <- get
  case popSome s of
    Nothing -> liftIO $ putStrLn "Heap is already empty"
    Just (n, heap) -> do
      put heap
      liftIO $ putStrLn (show n <> "\nNew size: " <> show (sizeSome heap))

add :: String -> Repl ()
add input =
  case readMaybe input of
    Nothing -> liftIO $ putStrLn ("Invalid input: " <> input)
    Just n -> do
      modify (addSome n)
      liftIO . putStrLn . ("New size: " <>) . show . sizeSome =<< get

colonCmd :: [(String, [String] -> Repl ())]
colonCmd =
  [ ("help", const help)
  , ("quit", const $ liftIO exitSuccess)
  ]

completer :: Monad m => WordCompleter m
completer n = return $ filter (isPrefixOf n) cmds
  where
    cmds = ["add", "pop", "print", "size", "help"]

banner :: Repl ()
banner =
  liftIO . putStrLn . unlines $
  [ "Braun Heap"
  , ""
  , "A simple CLI for manipulating an integer Braun heap with type-level constraints on subtrees"
  , ""
  , "Available commands: add, pop, print, size, help"
  ]

help :: Repl ()
help =
  liftIO . putStrLn . unlines $
  [ "Available commands:"
  , ""
  , "print          - pretty-print current state of the heap"
  , "size           - print current size of the heap"
  , "add X [Y Z...] - insert a list of numbers into the heap"
  , "pop            - remove a single number from the heap"
  , ":quit          - Quit the REPL"
  , ":help          - This text"
  ]
