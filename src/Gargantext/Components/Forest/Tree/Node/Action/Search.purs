module Gargantext.Components.Forest.Tree.Node.Action.Search where

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Lang (allLangs)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search"


type Props =
  ( dispatch  :: Action -> Aff Unit
  , id        :: Maybe ID
  , session   :: Session )

-- | Action : Search
actionSearch :: R2.Component Props
actionSearch = R.createElement actionSearchCpt
actionSearchCpt :: R.Component Props
actionSearchCpt = here.component "actionSearch" cpt
  where
    cpt { dispatch, id, session } _ = do
      search <- T.useBox $ defaultSearch { node_id = id }
      pure $ R.fragment [ H.p { className: "action-search" }
                              [ H.text $ "Search and create a private "
                                      <> "corpus with the search query as corpus name." ]
                        , searchBar { langs: allLangs
                                    , onSearch: searchOn dispatch
                                    , search
                                    , session
                                    } []
                        ]
        where
          searchOn :: (Action -> Aff Unit)
                  -> GT.AsyncTaskWithType
                  -> Effect Unit
          searchOn dispatch' task = do
            _ <- launchAff $ dispatch' (DoSearch task)
            -- close popup
            _ <- launchAff $ dispatch' ClosePopover
            -- TODO
            --snd p $ const Nothing
            pure unit
