{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import System.Exit (die)
import System.IO (readFile, getLine, putStr, putStrLn, hFlush, stdout)

import TuringData (TMachine(..), Tape(..))
import TuringFuncs (compute)
import TuringParse (parseTM)


main :: IO ()
main = do
  (runner, f) <- getArgs >>= \case
    ["-i", y] -> pure (dumpTM, y)
    ["-s", y] -> pure (simulateTM, y)
    [x, _] -> die ("unknown option " ++ x)
    _ -> die "expects two arguments: [-i|-s] FILE"
  parseTM <$> readFile f >>= \case
    Left e -> die e
    Right input -> runner input

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
  let (cfg, result) = compute tm (Tape headInp mempty tailInp)
  mapM_ print cfg
  putStrLn result
