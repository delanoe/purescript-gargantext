module Gargantext.Components.Forest.Tree.Node.Action.Search where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Lang (allLangs)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search"


type Props =
  ( boxes     :: Boxes
  , dispatch  :: Action -> Aff Unit
  , id        :: Maybe ID )

-- | Action : Search
actionSearch :: R2.Component Props
actionSearch = R.createElement actionSearchCpt
actionSearchCpt :: R.Component Props
actionSearchCpt = here.component "actionSearch" cpt
  where
    cpt { boxes: { errors }, dispatch, id } _ = do
      search <- T.useBox $ defaultSearch { node_id = id }
      pure $ R.fragment
        [ H.p { className: "action-search m-1" }
          [ H.text $ "Search and create a private "
            <> "corpus with the search query as corpus name." ]
        , searchBar { errors
                    , langs: allLangs
                    , onSearch: searchOn dispatch
                    , search
                    } []
        ]
        where
          searchOn :: (Action -> Aff Unit)
                   -> GT.AsyncTaskWithType
                   -> Effect Unit
          searchOn dispatch' task = do
            _ <- launchAff $ dispatch' (DoSearch task)
            -- close popup
            _ <- launchAff $ dispatch' CloseBox
            -- TODO
            --snd p $ const Nothing
            pure unit
