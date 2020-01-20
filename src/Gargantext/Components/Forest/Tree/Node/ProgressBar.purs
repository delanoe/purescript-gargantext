module Gargantext.Components.Forest.Tree.Node.ProgressBar where

import Data.Int (fromNumber)
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Effect.Timer (setTimeout)
import Math (min)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, discard, pure, show, unit, ($), (+), (<>))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action (ID)
import Gargantext.Types (AsyncTask(..))


type Props =
  (
    asyncTask :: AsyncTask
  , corpusId  :: ID
  )


asyncProgressBar :: Record Props -> R.Element
asyncProgressBar p = R.createElement asyncProgressBarCpt p []

asyncProgressBarCpt :: R.Component Props
asyncProgressBarCpt = R.hooksComponent "G.C.F.T.N.asyncProgressBar" cpt
  where
    cpt {asyncTask: (AsyncTask {id}), corpusId} _ = do
      (progress /\ setProgress) <- R.useState' 0.0

      R.useEffect' $ do
        _ <- setTimeout 1000 $ do
          setProgress \p -> min 100.0 (p + 10.0)
        pure unit


      pure $
        H.div { className: "progress" } [
          H.div { className: "progress-bar"
                , role: "progressbar"
                , style: { width: (show $ toInt progress) <> "%" }
                } [ H.text id ]
        ]

    toInt :: Number -> Int
    toInt n = unsafePartial $ fromJust $ fromNumber n
