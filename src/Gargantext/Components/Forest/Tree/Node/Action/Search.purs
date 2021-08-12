module Gargantext.Components.Forest.Tree.Node.Action.Search where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (NodePopup)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Lang (allLangs)
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError, ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search"


type Props =
  ( dispatch  :: Action -> Aff Unit
  , errors    :: T.Box (Array FrontendError)
  , id        :: Maybe ID
  , nodePopup :: Maybe NodePopup
  , session   :: Session )

-- | Action : Search
actionSearch :: R2.Component Props
actionSearch = R.createElement actionSearchCpt
actionSearchCpt :: R.Component Props
actionSearchCpt = here.component "actionSearch" cpt
  where
    cpt { dispatch, errors, id, nodePopup, session } _ = do
      search <- T.useBox $ defaultSearch { node_id = id }
      pure $ R.fragment [ H.p { className: "action-search" }
                              [ H.text $ "Search and create a private "
                                      <> "corpus with the search query as corpus name." ]
                        , searchBar { errors
                                    , langs: allLangs
                                    , onSearch: searchOn dispatch nodePopup
                                    , search
                                    , session
                                    } []
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
