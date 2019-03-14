{-# LANGUAGE RecordWildCards #-}

module TuringData where

import Control.Arrow (first)
import Control.Monad (join)
import Data.List (dropWhileEnd, intercalate, unfoldr)


type TState = String
type TSymbol = Char

-- Akce je posun nebo zápis symbolu
data Action
  = ALeft
  | ARight
  | AWrite TSymbol
  deriving (Eq)

instance Show Action where
  show ALeft = "<"
  show ARight = ">"
  show (AWrite c) = [c]

-- Pravidlo přechodové funkce
data Transition = Trans
  { fromState :: TState
  , fromSym :: TSymbol
  , toState :: TState
  , toAction :: Action
  } deriving (Eq)

instance Show Transition where
  show (Trans fq fs tq ta) = intercalate "," [fq, [fs], tq, show ta]

-- Celý Turingův stroj
data TMachine = TM
  { states :: [TState]
  , alphabet :: [TSymbol]
  , start :: TState
  , end :: TState
  , transRules :: [Transition]
  } deriving (Eq)

instance Show TMachine where
  show TM {..} =
    unlines $
    [intercalate "," states, alphabet, start, end] ++ map show transRules

-- Páska: symbol pod hlavou, symboly nalevo obráceně, symboly napravo
data Tape =
  Tape TSymbol
       [TSymbol]
       [TSymbol]

instance Show Tape where
  show (Tape x lts rts) =
    reverse (cut lts) ++ hili x ++ dropWhileEnd ('_' ==) (cut rts)
    where
      cut = take 30
      hili x = "[" ++ [x] ++ "]"
      -- hili x = "\ESC[34;1m" ++ [x] ++ "\ESC[0m"

-- Konfigurace Turingova stroje
data TMConfig = TMConf TState Tape

instance Show TMConfig where
  show (TMConf q tp) = "state " ++ q ++ "  tape: " ++ show tp


-- Typ výsledku nebo text případné chyby
type Err = Either String

-- Pomocná funkce; obdoba iterate, ale výsledek může být konečný seznam
-- Opakovaně aplikuje funkci na počáteční hodnotu
iterE :: (a -> Either b a) -> a -> ([a], b)
-- Cv: ZAPIŠTE DEFINICI POMOCÍ either
iterE g z =
  case g z of
    Left e -> ([z], e)
    Right x -> first (z :) (iterE g x)
