{-# LANGUAGE TupleSections #-}

module TuringFuncs (compute)  where

import Control.Arrow (first)
import Data.List (find)
import TuringData
  ( Action(..)
  , TMConfig(..)
  , TMachine(..)
  , Tape(..)
  , Transition(..)
  )


-- Výpočet TM je posloupnost konfigurací a výsledek
compute :: TMachine -> Tape -> ([TMConfig], String)
compute tm tape = iterE (nextConfig tm) (TMConf (start tm) tape)

-- Změna konfigurace jedním krokem výpočtu
nextConfig :: TMachine -> TMConfig -> Either String TMConfig
nextConfig tm cf@(TMConf q _) =
  if q == end tm
    then Left "accepted"
    else step cf =<< findRule cf (transRules tm)

-- Krok výpočtu podle pravidla přech. funkce
step :: TMConfig -> Transition -> Either String TMConfig
step (TMConf _ (Tape x (l:tpl) tpr)) (Trans _ _ q ALeft) =
  Right $ TMConf q (Tape l tpl (x:tpr))
step (TMConf _ (Tape x tpl (r:tpr))) (Trans _ _ q ARight) =
  Right $ TMConf q (Tape r (x:tpl) tpr)
step (TMConf _ (Tape _ tpl tpr)) (Trans _ _ q (AWrite w)) =
  Right $ TMConf q (Tape w tpl tpr)
step (TMConf _ (Tape _ [] _)) (Trans _ _ _ ALeft) =
  Left "Trying to move past the left end of the tape"
step (TMConf _ (Tape _ _ [])) _ =
  Left "Trying to move past the right ene of the tape"

-- Vyhledání pravidla v seznamu
findRule :: TMConfig -> [Transition] -> Either String Transition
findRule (TMConf q (Tape x _ _)) =
  maybe (Left "no transition") Right . find (\(Trans u c _ _) -> q == u && x == c)

-- Pomocná funkce; obdoba iterate, ale výsledek může být konečný seznam
-- Opakovaně aplikuje funkci na počáteční hodnotu
iterE :: (a -> Either b a) -> a -> ([a], b)
iterE g z = either ([z],) (first (z:) . iterE g) (g z)
