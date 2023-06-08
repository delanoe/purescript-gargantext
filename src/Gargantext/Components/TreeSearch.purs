module Gargantext.Components.TreeSearch where

import Gargantext.Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Gargantext.Components.Bootstrap (formSelect')
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ModalSizing(..), Position(..), TooltipPosition(..), Variant(..))
import Gargantext.Components.InputWithEnter (inputWithEnter)
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
, query :: T.Box String
)

type WrapperProps = (
  visible :: T.Box Boolean
, sessions :: Sessions
, session  :: T.Box (Maybe Session)
, query :: T.Box String
)

type ContainerProps = ( visible :: T.Box Boolean, session :: Session, query :: String)

type RenderContainerProps = ( visible :: T.Box Boolean, session :: Session, searchData :: Array SearchData )

type RenderProps = ( visible        :: T.Box Boolean
                   , session        :: Session
                   , searchData     :: Array SearchData
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
    query <- T.useBox ""
    inputRef <- R.useRef ""

    pure $ 
      B.baseModal
      { isVisibleBox: visible
      , title: Just "Search in tree"
      , size: MediumModalSize
      , modalClassName: ""
      }
      [ 
        H.div
        { className: "input-group p-1" }
        [
          inputWithEnter { className: "form-control"
                       , autoFocus: true
                       , onEnter: submit inputRef query
                       , onValueChanged: R.setRef inputRef
                       , onBlur: R.setRef inputRef
                       , type: "value"
                       , defaultValue: ""
                       , required: true
                       , placeholder: "Search keys..."
                       }
        ,
          H.div { className: "input-group-append"}
          [
            H.button { className: "input-group-text btn btn-light text-secondary"
                    , on: { click: submit inputRef query }
                    , type: "submit" }
            [ H.span {className: "fa fa-search"} [] ]
          ]
        ]
      , treeSearchState {visible, query, sessions: sessions'} 
      ]
      where
        submit ref box _ = T.write_ (R.readRef ref) box

treeSearchState :: R2.Leaf StateProps
treeSearchState = R2.leaf treeSearchStateCpt

treeSearchStateCpt :: R.Component StateProps
treeSearchStateCpt = here.component "treeSearchState" cpt where
  cpt { query, sessions, visible } _ = do
    session <- T.useBox $ head $ unSessions sessions

    pure $ treeSearchWrapper { query, visible, session, sessions}

treeSearchWrapper :: R2.Leaf WrapperProps
treeSearchWrapper = R2.leaf treeSearchWrapperCpt

treeSearchWrapperCpt :: R.Component WrapperProps
treeSearchWrapperCpt = here.component "treeSearchWrapper" cpt where
  cpt { query, visible, sessions, session } _ = do
    session' <- T.useLive T.unequal session
    query' <- T.useLive T.unequal query

    case session' of
      Just s ->
        pure $ H.div {className: "search-modal__results-wrapper px-1"} 
        [ formSelect'
            { callback: \v -> T.write_ (Just v) session
            , list: unSessions sessions
            , value: s
            } []
        ,
          H.hr {}
        , 
          if query' == "" then
            H.div {className: "search-modal__results mb-1"} [B.span_ "Please enter your search query..."]
          else
            treeSearchContainer {query: query', visible, session: s}
        ]
      Nothing -> pure $ H.div {} []

treeSearchContainer :: R2.Leaf ContainerProps
treeSearchContainer = R2.leaf treeSearchContainerCpt

treeSearchContainerCpt :: R.Component ContainerProps
treeSearchContainerCpt = here.component "treeSearchContainerCpt" cpt where
  cpt {query, visible, session } _ = do

    useLoader { errorHandler
              , path: { session, query }
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
    pure $ treeSearchRender { visible, session, searchData, goToRoute }

treeSearchRender :: R2.Leaf RenderProps
treeSearchRender = R2.leaf treeSearchRenderCpt

treeSearchRenderCpt :: R.Component RenderProps
treeSearchRenderCpt = here.component "treeSearchRenderCpt" cpt where
  cpt { visible, session, searchData, goToRoute } _ = do

    pure $ H.div {className: "search-modal__results"} (results searchData)
      where
        results s = map searchResult s
          where
            searchResult sd = H.div 
                                { className: "result mt-1"} 
                                [ 
                                  B.tooltipContainer
                                  { delayShow: 600
                                  , position: TooltipPosition Right
                                  , tooltipSlot: B.span_ $ show $ fromMaybe Home $ nodeTypeAppRoute sd.type (sessionId session) sd.id
                                  , defaultSlot:
                                    B.button 
                                    { className: "result__button"
                                    , callback: \_ -> do 
                                      T.write_ false visible
                                      goToRoute $ fromMaybe Home $ nodeTypeAppRoute sd.type (sessionId session) sd.id 
                                    , variant: ButtonVariant Light }
                                    [
                                      B.icon {name: getIcon sd.type true}
                                    , B.wad_ [ "d-inline-block", "w-1" ] 
                                    , H.text sd.name
                                    ]
                                  }
                                ]

type LoadProps = ( session :: Session, query :: String )

loadSearch :: Record LoadProps -> AffRESTError (Array SearchData)
loadSearch { session: s, query: q} = get s $ appPath (TreeFlat (sessionId s) (sessionRoot s) q)
  where sessionRoot (Session {treeId}) = treeId
