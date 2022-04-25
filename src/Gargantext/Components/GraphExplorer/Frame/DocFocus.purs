module Gargantext.Components.GraphExplorer.Frame.DocFocus
  ( docFocus
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Gargantext.Components.GraphExplorer.Types (GraphSideDoc(..))
import Gargantext.Components.Nodes.Corpus.Document (documentMainLayout)
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H


here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Frame.DocFocus"

type Props =
  ( graphSideDoc  :: GraphSideDoc
  , session       :: Session
  )

docFocus :: R2.Leaf Props
docFocus = R2.leaf docFocusCpt

docFocusCpt :: R.Component Props
docFocusCpt = here.component "main" cpt where
  cpt { graphSideDoc: GraphSideDoc { docId, listId, corpusId }
      , session
      } _ = do


    -- | Render
    -- |
    pure $

      H.div
      { className: "graph-layout__focus" }
      [
        H.div
        { className: "graph-layout__focus__inner" }
        [
          documentMainLayout
          { listId
          , mCorpusId: Just corpusId
          , nodeId: docId
          , session
          }
          []
        ]
      ]
