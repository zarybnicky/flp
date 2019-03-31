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
      rec addInput <- inputElement $ def & inputElementConfig_setValue .~ ("" <@ eAdd')
          eAdd <- button "Add"
          let eAdd' = fmapMaybe (readMaybe . T.unpack) $
                current (_inputElement_value addInput) <@ eAdd <> keypress Enter addInput
      ePop <- button "Pop"

      heap :: R.Dynamic t (SomeHeap Int) <- foldDyn ($) (SomeHeap empty) $ mergeWith (.)
        [ addSome <$> eAdd'
        , maybe (SomeHeap empty) snd . popSome <$ ePop
        ]
      dLog <- foldDyn (:) [] $ leftmost
        [ ("Added element: " <>) . T.pack . show <$> eAdd'
        , ("Popped element: " <>) . T.pack . show <$> fmapMaybe topSome (current heap <@ ePop)
        ]
      _ <- R.el "ul" $ simpleList dLog (R.el "li" . dynText)
      pure heap

    rec
      (e, _) <- elClass' "div" "display" $ do
        window <- liftJSM DOM.currentWindowUnchecked
        ePostBuild <- delay 0.3 =<< getPostBuild
        eResize <- wrapDomEvent window (`DOM.on` DOM.resize) (pure ())
        eSize <- performEvent . ffor (ePostBuild <> eResize) $ \() -> do
          let e' = uncheckedCastTo DOM.HTMLElement $ _element_raw e
          w <- DOM.getOffsetWidth e'
          h <- DOM.getOffsetHeight e'
          pure (w, h)
        dSize <- holdDyn (0, 0) eSize

        void . dyn $ ffor2 dSize dHeap $ \(w, h) heap ->
          reflexDia (def & sizeSpec .~ dims2D w h) (diagTree heap)
    pure ()

topSome :: SomeHeap a -> Maybe a
topSome (SomeHeap BH.Empty) = Nothing
topSome (SomeHeap (Node _ _ x _)) = Just x

diagTree :: (Show s) => SomeHeap s -> Diagram B
diagTree (SomeHeap h) = go [] h where
  go :: (Show s) => String -> Heap m s -> Diagram B
  go nm BH.Empty = diagNode "∅" # named nm
  go nm (Node _ l x r) =
    connectOutside nm nmL .
    connectOutside nm nmR $
      nx === (nl ||| nr) # centerX
    where
      (nmL, nmR) = ('L':nm, 'R':nm)
      nx = diagNode (show x) # named nm
      nl = go nmL l # named nmL
      nr = go nmR r # named nmR

diagNode :: String -> Diagram B
diagNode txt = (D.text txt <> circle 1 # pad 2 . fc white) # fontSizeL 0.4
