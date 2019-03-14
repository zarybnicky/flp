module Main (main)  where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile, getLine, putStr, putStrLn, hFlush, stdout)

import TuringData (TMachine(..), TMConfig(..), Tape(..))
import TuringFuncs (computation)
import TuringParse (parseTM)


main :: IO ()
main = do
  (action, input) <- procArgs =<< getArgs
  either die action (parseTM input)

-- Zpracování příkazového řádku
procArgs :: [String] -> IO (TMachine -> IO (), String)
procArgs [x, y] = do
  input <- readFile y
  case x of
    "-i" -> return (dumpTM, input)
    "-s" -> return (simulateTM, input)
    _ -> die ("unknown option " ++ x)
procArgs _ = die "expects two arguments: [-i|-s] FILE"

-- Výpis na stdout při volbě '-i'
dumpTM :: TMachine -> IO ()
dumpTM tm = do
  putStrLn "dumping TM ..."
  putStr (show tm)

-- Načtení pásky a simulace TM při volbě '-s'
simulateTM :: TMachine -> IO ()
simulateTM tm = do
  putStr "input tape: " >> hFlush stdout
  headInp:tailInp <- (++ repeat '_') <$> getLine
  putStrLn "simulating TM ..."
  -- Cv: DOPLŇTE SPRÁVNÉ ARGUMENTY MÍSTO undefined
  printComp (computation undefined undefined)
  where
    printComp comp = do
      mapM_ print (fst comp)
      putStrLn (snd comp)
