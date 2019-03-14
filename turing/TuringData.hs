{-# LANGUAGE RecordWildCards #-}

module TuringData
  ( TState
  , TSymbol
  , Action(..)
  , Transition(..)
  , TMachine(..)
  , Tape(..)
  , TMConfig(..)
  ) where

import Data.List (dropWhileEnd, intercalate)


type TState = String
type TSymbol = Char

-- Akce je posun nebo zápis symbolu
data Action
  = ALeft
  | ARight
  | AWrite TSymbol
  deriving (Eq)

-- Pravidlo přechodové funkce
data Transition = Trans
  { fromState :: TState
  , fromSym :: TSymbol
  , toState :: TState
  , toAction :: Action
  } deriving (Eq)

-- Celý Turingův stroj
data TMachine = TM
  { states :: [TState]
  , alphabet :: [TSymbol]
  , start :: TState
  , end :: TState
  , transRules :: [Transition]
  } deriving (Eq)

-- Páska: symbol pod hlavou, symboly nalevo obráceně, symboly napravo
data Tape = Tape TSymbol [TSymbol] [TSymbol]

-- Konfigurace Turingova stroje
data TMConfig = TMConf TState Tape


instance Show Action where
  show ALeft = "<"
  show ARight = ">"
  show (AWrite c) = [c]

instance Show Transition where
  show (Trans fq fs tq ta) = intercalate "," [fq, [fs], tq, show ta]

instance Show TMachine where
  show TM {..} =
    unlines $
    [intercalate "," states, alphabet, start, end] ++ map show transRules

instance Show Tape where
  show (Tape x lts rts) =
    reverse (cut lts) ++ hili x ++ dropWhileEnd ('_' ==) (cut rts)
    where
      cut = take 30
      hili y = "[" ++ [y] ++ "]"

instance Show TMConfig where
  show (TMConf q tp) = "state " ++ q ++ "  tape: " ++ show tp
