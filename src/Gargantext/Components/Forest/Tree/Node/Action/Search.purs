module Gargantext.Components.Forest.Tree.Node.Action.Search where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchBar (searchBar)
import Gargantext.Components.Forest.Tree.Node.Action.Search.SearchField (defaultSearch)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Utils (loadLanguages)
import Gargantext.Components.Lang (allLangs, Lang)
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search"


type Props =
  ( boxes     :: Boxes
  , dispatch  :: Action -> Aff Unit
  , id        :: Maybe ID
  , session   :: Session )

-- | Action : Search
actionSearch :: R2.Component Props
actionSearch = R.createElement actionSearchCpt
actionSearchCpt :: R.Component Props
actionSearchCpt = here.component "actionSearch" cpt where
  cpt props@({ session }) _ = do
    useLoader { errorHandler
              , loader: loadLanguages
              , path: { session }
              , render: \langs ->
                 actionSearchWithLangs (Record.merge props { langs }) [] }
    where
      errorHandler err = case err of
        ReadJSONError err' -> here.warn2 "[actionSearch] ReadJSONError" $ show err'
        _                  -> here.warn2 "[actionSearch] RESTError" err

type PropsWithLangs =
  ( langs :: Array Lang
  | Props )

-- | Action : Search
actionSearchWithLangs :: R2.Component PropsWithLangs
actionSearchWithLangs = R.createElement actionSearchWithLangsCpt
actionSearchWithLangsCpt :: R.Component PropsWithLangs
actionSearchWithLangsCpt = here.component "actionSearchWithLangs" cpt
  where
    cpt { boxes: { errors }, dispatch, id, langs, session } _ = do
      search <- T.useBox $ defaultSearch { node_id = id }
      pure $ R.fragment
        [ H.p { className: "action-search mx-2" }
          [ H.text $ "Search and create a private "
            <> "corpus with the search query as corpus name." ]
        , searchBar { errors
                    , langs
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
            _ <- launchAff $ dispatch' CloseBox
            -- TODO
            --snd p $ const Nothing
            pure unit
