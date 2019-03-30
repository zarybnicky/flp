{-# LANGUAGE GADTs #-}

module Main where

import Diagrams.Prelude as D
import Diagrams.Backend.Reflex
import BraunHeap.TypeLits as BH
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core

main :: IO ()
main = run 3000 $ mainWidgetWithCss mempty $ do
  diaDyn <- holdDyn (diagTree (singleton (5 :: Int))) never
  _ <- dyn $ reflexDia (def & sizeSpec .~ dims2D 500 1000) <$> diaDyn
  return ()

diagTree :: (Show s) => Heap n s -> Diagram B
diagTree = go [] where
  go :: (Show s) => String -> Heap m s -> Diagram B
  go nm BH.Empty = diagNode "Empty" # named nm
  go nm (Node _ l x r) =
    connectOutside nm nmL .
    connectOutside nm nmR $
      nx
      ===
      (nl ||| nr) # centerX
    where
      (nmL, nmR) = ('L':nm, 'R':nm)
      nx = diagNode (show x) # named nm
      nl = go nmL l # named nmL
      nr = go nmR r # named nmR

diagNode :: String -> Diagram B
diagNode txt = D.text txt # fontSizeL 0.4 `atop` circle 1 # pad 2 . fc white
