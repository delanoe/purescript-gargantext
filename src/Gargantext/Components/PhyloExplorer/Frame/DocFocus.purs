module Gargantext.Components.PhyloExplorer.Frame.DocFocus
  ( docFocus
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Nodes.Corpus.Document (node)
import Gargantext.Components.PhyloExplorer.Types (FrameDoc(..))
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H


here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Frame.DocFocus"

type Props =
  ( frameDoc      :: FrameDoc
  , session       :: Session
  , closeCallback :: Unit -> Effect Unit
  )

docFocus :: R2.Leaf Props
docFocus = R2.leaf docFocusCpt

docFocusCpt :: R.Component Props
docFocusCpt = here.component "main" cpt where
  cpt { frameDoc: FrameDoc { docId, listId, corpusId }
      , session
      , closeCallback
      } _ = do
    -- | Render
    -- |
    pure $

      H.div
      { className: "graph-doc-focus" }
      [
        H.div
        { className: "graph-doc-focus__header" }
        [
          B.iconButton
          { name: "times"
          , elevation: Level2
          , callback: closeCallback
          }
        ]
      ,
        H.div
        { className: "graph-doc-focus__body" }
        [
          -- print the document node
          node
          { listId
          , mCorpusId: Just corpusId
          , nodeId: docId
          , key: show (sessionId session) <> "-" <> show docId
          }
        ]
      ]
