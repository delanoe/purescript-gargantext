module Gargantext.Components.TreeSearch where

import Gargantext.Prelude

import DOM.Simple as DOM
import Data.Array (filter, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, null)
import Data.String (Pattern(..), contains, toLower)
import Effect (Effect)
import Gargantext.Components.Bootstrap (formSelect')
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ModalSizing(..))
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute(..), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session(..), Sessions, get, sessionId, unSessions)
import Gargantext.Sessions.Types (Session)
import Gargantext.Types (NodeID, NodeType, getIcon)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.TreeSearch"

type Props = (
  visible :: T.Box Boolean
, sessions :: T.Box Sessions
)

type StateProps = (
  visible :: T.Box Boolean
, sessions :: Sessions
)

type WrapperProps = (
  visible :: T.Box Boolean
, sessions :: Sessions
, session  :: T.Box (Maybe Session)
)

type ContainerProps = ( visible :: T.Box Boolean, session :: Session )

type RenderContainerProps = ( visible :: T.Box Boolean, session :: Session, searchData :: Array SearchData )

type RenderProps = ( visible        :: T.Box Boolean
                   , session        :: Session
                   , filteredData   :: T.Box (Array SearchData)
                   , searchData     :: Array SearchData
                   , inputRef       :: R.Ref (Nullable DOM.Element)
                   , goToRoute      :: AppRoute -> Effect Unit )

type SearchData = { name :: String
                  , type :: NodeType
                  , id   :: NodeID
}

treeSearch :: R2.Leaf Props
treeSearch = R2.leaf treeSearchCpt

treeSearchCpt :: R.Component Props
treeSearchCpt = here.component "treeSearch" cpt where
  cpt { sessions, visible } _ = do
    sessions' <- T.useLive T.unequal sessions

    pure $ 
      B.baseModal
      { isVisibleBox: visible
      , title: Just "Tree Search"
      , size: ExtraLargeModalSize
      , modalClassName: ""
      }
      [  treeSearchState {visible, sessions: sessions'} ]

treeSearchState :: R2.Leaf StateProps
treeSearchState = R2.leaf treeSearchStateCpt

treeSearchStateCpt :: R.Component StateProps
treeSearchStateCpt = here.component "treeSearchState" cpt where
  cpt { sessions, visible } _ = do
    session <- T.useBox $ head $ unSessions sessions

    pure $ treeSearchWrapper { visible, session, sessions}

treeSearchWrapper :: R2.Leaf WrapperProps
treeSearchWrapper = R2.leaf treeSearchWrapperCpt

treeSearchWrapperCpt :: R.Component WrapperProps
treeSearchWrapperCpt = here.component "treeSearchWrapper" cpt where
  cpt { visible, sessions, session } _ = do
    session' <- T.useLive T.unequal session

    case session' of
      Just s ->
        pure $ R.fragment [
          formSelect'
            { callback: \v -> T.write_ (Just v) session
            , list: unSessions sessions
            , value: s
            } []
        , 
          treeSearchContainer {visible, session: s}
        ]
      Nothing -> pure $ H.div {} []

treeSearchContainer :: R2.Leaf ContainerProps
treeSearchContainer = R2.leaf treeSearchContainerCpt

treeSearchContainerCpt :: R.Component ContainerProps
treeSearchContainerCpt = here.component "treeSearchContainerCpt" cpt where
  cpt {visible, session } _ = do

    useLoader { errorHandler
              , path: { session }
              , loader: loadSearch
              , render: \searchData -> treeSearchRenderContainer { visible, session, searchData }
    }
    where
      errorHandler = logRESTError here "[treeSearchContainer]"

treeSearchRenderContainer :: R2.Leaf RenderContainerProps
treeSearchRenderContainer = R2.leaf treeSearchRenderContainerCpt

treeSearchRenderContainerCpt :: R.Component RenderContainerProps
treeSearchRenderContainerCpt = here.component "treeSearchRenderContainer" cpt where
  cpt { visible, session, searchData } _ = do
    { goToRoute } <- useLinkHandler
    filteredData <- T.useBox searchData
    inputRef <- R.useRef null
    pure $ treeSearchRender { visible, session, filteredData, searchData, inputRef, goToRoute }

treeSearchRender :: R2.Leaf RenderProps
treeSearchRender = R2.leaf treeSearchRenderCpt

treeSearchRenderCpt :: R.Component RenderProps
treeSearchRenderCpt = here.component "treeSearchRenderCpt" cpt where
  cpt { visible, session, filteredData, searchData, inputRef, goToRoute } _ = do
    filteredData' <- T.useLive T.unequal filteredData

    pure $ H.div {} [
      H.div {} [H.input { className: "form-control"
                        , name: "search"
                        , ref: inputRef
                        , on: {change: change }
                        , type: "value"
                        , placeholder: "Search..."
                        }]
    , H.div {} $ results filteredData'
    ]
      where
        results s = map searchResult s
          where
            searchResult sd = H.div {} [H.button { className: "mainleaf"
                                                 , on: { click: do 
                                                          T.write_ false visible
                                                          goToRoute $ fromMaybe Home $ nodeTypeAppRoute sd.type (sessionId session) sd.id}} 
                                                 [B.icon {name: getIcon sd.type true} , H.text sd.name]]
        change _ = T.write_ (filter (\val -> contains (Pattern $ toLower $ R2.getInputValue inputRef) (toLower val.name)) searchData) filteredData

type LoadProps = ( session :: Session )

loadSearch :: Record LoadProps -> AffRESTError (Array SearchData)
loadSearch { session: s } = get s $ appPath (TreeFlat (sessionId s) (sessionRoot s))
  where sessionRoot (Session {treeId}) = treeId
