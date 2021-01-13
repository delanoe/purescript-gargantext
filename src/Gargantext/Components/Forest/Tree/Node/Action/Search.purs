module Gargantext.Components.Forest.Tree.Node.Action.Search where

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Lang (allLangs)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Types as GT


-- | Action : Search
actionSearch :: Session
            -> Maybe ID
            -> (Action -> Aff Unit)
            -> Maybe NodePopup
            -> R.Hooks R.Element
actionSearch session id dispatch nodePopup = do
  search <- R.useState' $ defaultSearch { node_id = id }
  pure $ R.fragment [ H.p { className: "action-search" }
                          [ H.text $ "Search and create a private "
                                  <> "corpus with the search query as corpus name." ]
                    , searchBar { langs: allLangs
                                , onSearch: searchOn dispatch nodePopup
                                , search
                                , session
                                }
                    ]
    where
      searchOn :: (Action -> Aff Unit)
               -> Maybe NodePopup
               -> GT.AsyncTaskWithType
               -> Effect Unit
      searchOn dispatch' p task = do
        _ <- launchAff $ dispatch' (DoSearch task)
        -- close popup
        _ <- launchAff $ dispatch' ClosePopover
        -- TODO
        --snd p $ const Nothing
        pure unit
