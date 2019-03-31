{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import BraunHeap.TypeLits as BH
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import Diagrams.Prelude as D
import Diagrams.Backend.Reflex
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import qualified GHCJS.DOM.HTMLElement as DOM
import GHCJS.DOM.Types (liftJSM, uncheckedCastTo)
import Language.Javascript.JSaddle.Warp (run)
import Reflex.Dom.Core as R
import Text.Read (readMaybe)

main :: IO ()
main = run 3000 $ mainWidgetWithCss css app

css :: ByteString
css = mconcat
  [ "html, body { height: 100%; }"
  , ".container { width: 100%; height: 100%; display: flex; }"
  , ".controls { flex: 1; }"
  , ".display { flex: 1; }"
  ]

app :: MonadWidget t m => m ()
app =
  divClass "container" $ do
    dHeap <- divClass "controls" $ do
      -- Draw control buttons
      rec addInput <- inputElement $ def & inputElementConfig_setValue .~ ("" <@ eAdd')
          eAdd <- button "Add"
          let eAdd' = fmapMaybe (readMaybe . T.unpack) $
                current (_inputElement_value addInput) <@ eAdd <> keypress Enter addInput
      ePop <- button "Pop"

      -- Create a heap based on the above inputs
      heap :: R.Dynamic t (SomeHeap Int) <- foldDyn ($) (SomeHeap empty) $ mergeWith (.)
        [ addSome <$> eAdd'
        , maybe (SomeHeap empty) snd . popSome <$ ePop
        ]

      -- Write out a log of performed actions (add/pop)
      dLog <- foldDyn (:) [] $ leftmost
        [ ("Added element: " <>) . T.pack . show <$> eAdd'
        , ("Popped element: " <>) . T.pack . show <$> fmapMaybe topSome (current heap <@ ePop)
        ]
      _ <- R.el "ul" $ simpleList dLog (R.el "li" . dynText)
      pure heap

    rec
      (e, _) <- elClass' "div" "display" . void $ do
        -- To fit <svg> into its parent element, we need to redraw on window resize
        dSize <- elSizeDyn e

        -- And finally draw the heap SVG
        dyn $ ffor2 dSize dHeap $ \(w, h) x -> reflexDia (def & sizeSpec .~ dims2D w h) (diagTree x)
    pure ()

-- | Given an 'Element', create a 'Dynamic' containing its size recalculated on
-- window resize.
elSizeDyn ::
     MonadWidget t m
  => Element EventResult GhcjsDomSpace t
  -> m (R.Dynamic t (Double, Double))
elSizeDyn e = do
  let e' = uncheckedCastTo DOM.HTMLElement $ _element_raw e
  window <- liftJSM DOM.currentWindowUnchecked
  ePostBuild <- delay 0.3 =<< getPostBuild
  eResize <- wrapDomEvent window (`DOM.on` DOM.resize) (pure ())
  eSize <- performEvent . ffor (ePostBuild <> eResize) $ \() -> do
    w <- DOM.getOffsetWidth e'
    h <- DOM.getOffsetHeight e'
    pure (w, h)
  holdDyn (0, 0) eSize

-- | Get the top element of 'SomeHeap'
topSome :: SomeHeap a -> Maybe a
topSome (SomeHeap BH.Empty) = Nothing
topSome (SomeHeap (Node _ _ x _)) = Just x

-- | Draw a tree diagram of a 'SomeHeap' object
diagTree :: (Show s) => SomeHeap s -> Diagram B
diagTree (SomeHeap h) = go [] h where
  go :: (Show s) => String -> Heap m s -> Diagram B
  go name BH.Empty = diagNode "∅" # named name
  go name (Node _ l x r) =
    connectOutside name nameL . connectOutside name nameR $
      node === (subL ||| subR) # centerX
    where
      (nameL, nameR) = ('L':name, 'R':name)
      node = diagNode (show x) # named name
      subL = go nameL l # named nameL
      subR = go nameR r # named nameR
  diagNode txt = (D.text txt <> circle 1 # pad 2 . fc white) # fontSizeL 0.4
