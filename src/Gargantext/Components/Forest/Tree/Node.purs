module Gargantext.Components.Forest.Tree.Node
  ( nodeSpan
  , blankNodeSpan
  ) where

import Gargantext.Prelude

import Data.Array.NonEmpty as NArray
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe)
import Data.String.Regex as Regex
import Data.Symbol (SProxy(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), Elevation(..), TooltipEffect(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload (DroppedFile(..), fileTypeView)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Settings (SettingsBox(..), settingsBox)
import Gargantext.Components.Forest.Tree.Node.Tools.Sync (nodeActionsGraph, nodeActionsNodeList)
import Gargantext.Components.GraphExplorer.API as GraphAPI
import Gargantext.Components.Lang (Lang(EN))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Context.Progress (asyncContext, asyncProgress)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Version (Version, useVersion)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (ID, Name)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp, textEllipsisBreak, (?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

-- (?) never been able to properly declare PureScript Regex...
foreign import nodeUserRegexp :: Regex.Regex

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node"

-- Main Node
type NodeSpanProps =
  ( boxes         :: Boxes
  , dispatch      :: Action -> Aff Unit
  , folderOpen    :: T.Box Boolean
  , frontends     :: Frontends
  , id            :: ID
  , isLeaf        :: IsLeaf
  , name          :: Name
  , nodeType      :: GT.NodeType
  , reload        :: T2.ReloadS
  , root          :: ID
  , session       :: Session
  , isBoxVisible  :: T.Box Boolean
  )

type IsLeaf = Boolean


nodeSpan :: R2.Leaf NodeSpanProps
nodeSpan = R2.leaf nodeSpanCpt
nodeSpanCpt :: R.Component NodeSpanProps
nodeSpanCpt = here.component "nodeSpan" cpt
  where
    cpt props@{ boxes: boxes@{ errors
                             , reloadMainPage
                             , reloadRoot
                             , route
                             , tasks }
              , dispatch
              , folderOpen
              , frontends
              , id
              , isLeaf
              , nodeType
              , reload
              , session
              , isBoxVisible
              } _ = do
    -- States

      route' <- T.useLive T.unequal route
      -- only 1 popup at a time is allowed to be opened
      droppedFile   <- T.useBox (Nothing :: Maybe DroppedFile)
      droppedFile'  <- T.useLive T.unequal droppedFile
      isDragOver    <- T.useBox false
      isDragOver'   <- T.useLive T.unequal isDragOver

      currentTasks  <- GAT.focus id tasks
      currentTasks' <- T.useLive T.unequal currentTasks

      folderOpen' <- R2.useLive' folderOpen

      -- tasks' <- T.read tasks

    -- Computed
      let

        dropClass :: Maybe DroppedFile -> Boolean -> String
        dropClass (Just _) _    = "mainleaf--file-dropped"
        dropClass _        true = "mainleaf--file-dropped"
        dropClass Nothing  _    = ""

        name' :: String -> GT.NodeType -> Session -> String
        name' _ GT.NodeUser s = show s
        name' n _           _ = n

        isSelected = Just route' == Routes.nodeTypeAppRoute nodeType (sessionId session) id

        SettingsBox {show: showBox} = settingsBox nodeType

        href = url frontends $ GT.NodePath (sessionId session) nodeType (Just id)

        name = name' props.name nodeType session

    -- Methods

        dropHandler :: forall event.
             SE.SyntheticEvent_ event
          -> Effect Unit
        dropHandler e = do
          -- prevent redirection when file is dropped
          SE.preventDefault e
          SE.stopPropagation e
          blob <- R2.dataTransferFileBlob e
          void $ launchAff do
            --contents <- readAsText blob
            liftEffect $ do
              T.write_ (Just
                      $ DroppedFile { blob: (UploadFileBlob blob)
                                    , fileType: Just CSV
                                    , lang    : EN
                                    }) droppedFile

        onDragOverHandler :: forall event.
             T.Box Boolean
          -> SE.SyntheticEvent_ event
          -> Effect Unit
        onDragOverHandler box e = do
          -- prevent redirection when file is dropped
          -- https://stackoverflow.com/a/6756680/941471
          SE.preventDefault e
          SE.stopPropagation e
          T.write_ true box

        onDragLeave :: forall event.
             T.Box Boolean
          -> SE.SyntheticEvent_ event
          -> Effect Unit
        onDragLeave box _ = T.write_ false box

        onTaskFinish ::
             GT.NodeID
          -> GT.AsyncTaskWithType
          -> Unit
          -> Effect Unit
        onTaskFinish id' t _ = do
          GAT.finish id' t tasks
          if GAT.asyncTaskTTriggersAppReload t then do
            here.log2 "reloading root for task" t
            T2.reload reloadRoot
          else do
            if GAT.asyncTaskTTriggersTreeReload t then do
              here.log2 "reloading tree for task" t
              T2.reload reload
            else do
              here.log2 "task doesn't trigger a tree reload" t
              pure unit
            if GAT.asyncTaskTTriggersMainPageReload t then do
              here.log2 "reloading main page for task" t
              T2.reload reloadMainPage
            else do
              here.log2 "task doesn't trigger a main page reload" t
              pure unit
          -- snd tasks $ GAT.Finish id' t
          -- mT <- T.read tasks
          -- case mT of
          --   Just t' -> snd t' $ GAT.Finish id' t
          --   Nothing -> pure unit
          -- T2.reload reloadRoot

        -- NOTE Don't toggle tree if it is not selected
        onNodeLinkClick :: Unit -> Effect Unit
        onNodeLinkClick _ = when (not isSelected) (T.write_ true folderOpen)

    -- Hooks

      mVersion <- useVersion $ nodeType == GT.NodeUser ?
        Just { session } $
        Nothing

      host <- R2.getPortalHost

    -- Render

      pure $

        H.span
        { className: intercalate " "
            [ "mainleaf"
            , dropClass droppedFile' isDragOver'
            , isSelected ? "mainleaf--selected" $ ""
            ]
        , on: { dragLeave: onDragLeave isDragOver
              , dragOver: onDragOverHandler isDragOver
              , drop: dropHandler
              }
        }
        [

      -- // Abstract informations //

          nodeTooltip
          { id
          , nodeType
          , name
          }
          [
            case mVersion of
              Nothing -> mempty
              Just v  -> versionComparator v
          ]
        ,
          R.createPortal
          [
            fileTypeView
            { dispatch
            , droppedFile
            , id
            , isDragOver
            , nodeType
            , key: "fileType-" <> show id
            }
          ]
          host
        ,

      -- // Leaf informations data //

          folderIcon
          { isLeaf
          , isOpened: folderOpen'
          , callback: const $ T.modify_ (not) folderOpen
          }
        ,
          nodeIcon
          (
            { nodeType
            , isLeaf
            , callback: const $ T.modify_ (not) folderOpen
            , isSelected
            }
          )
          [
            case mVersion of
              Nothing                              -> mempty
              Just { clientVersion, remoteVersion} ->
                B.iconButton $
                { className: intercalate " "
                    [ "mainleaf__version-badge"
                    , clientVersion == remoteVersion ?
                        "mainleaf__version-badge--matched" $
                        "mainleaf__version-badge--mismatched"
                    ]
                , name: clientVersion == remoteVersion ?
                    "check-circle" $
                    "exclamation-circle"
                , callback: const $ T.modify_ (not) folderOpen
                , overlay: false
                }
          ]
        ,
          nodeLink
          { callback: onNodeLinkClick
          , href
          , id
          , name: name' props.name nodeType session
          , type: nodeType
          }
        ,

      -- // Leaf action features //

          nodeActions
          { id
          , nodeType
          , refresh: const $ dispatch RefreshTree
          , session
          } []
        ,
          R2.when (showBox) $

            B.iconButton
            { name: "cog"
            , className: "mainleaf__settings-icon"
            , callback: \_ -> T.write_ true isBoxVisible
            , title:
                  "Each node of the Tree can perform some actions.\n"
                <> "Click here to execute one of them."
            , variant: Secondary
            , elevation: Level1
            }
        ,
          R.fragment $ flip map currentTasks' \task ->

            asyncProgress
            { asyncTask: task
            , errors
            , nodeId: id
            , onFinish: onTaskFinish id task
            , session
            }
            [
              taskProgress
              {}
            ]
          ,

        -- // Modals //

          B.baseModal
          { isVisibleBox: isBoxVisible
          , noBody: true
          , noHeader: true
          , modalClassName: "forest-tree-node-modal"
          }
          [
            nodePopupView
            { boxes
            , dispatch
            , id
            , name
            , nodeType
            , closeCallback: \_ -> T.write_ false isBoxVisible
            , session
            }
          ]
        ]


---------------------------------------------------------

type NodeIconProps =
  ( nodeType      ::  GT.NodeType
  , callback      :: Unit -> Effect Unit
  , isLeaf        :: Boolean
  , isSelected    :: Boolean
  )

nodeIcon :: R2.Component NodeIconProps
nodeIcon = R2.component nodeIconCpt
nodeIconCpt :: R.Component NodeIconProps
nodeIconCpt = here.component "nodeIcon" cpt where
  cpt { nodeType
      , callback
      , isLeaf
      , isSelected
      } children = do
    -- Render
    pure $

      H.span
      { className: "mainleaf__node-icon" } $
      [
        B.iconButton
        { name: GT.getIcon nodeType true
        , callback
        , status: isLeaf ? Idled $ Enabled
        , variant: isSelected ? Primary $ Dark
        , overlay: false
        }
      ]
        <> children

-----------------------------------------------

type FolderIconProps =
  ( isOpened :: Boolean
  , callback :: Unit -> Effect Unit
  , isLeaf   :: Boolean
  )

folderIcon :: R2.Leaf FolderIconProps
folderIcon = R2.leaf folderIconCpt
folderIconCpt :: R.Component FolderIconProps
folderIconCpt = here.component "folderIcon" cpt where
  cpt { isLeaf: true } _ = pure $

    B.icon
    { className: intercalate " "
        ["mainleaf__folder-icon"
        , "mainleaf__folder-icon--leaf"
        ]
    , name: "caret-right"
    }

  cpt { callback, isOpened } _ = pure $

    B.iconButton
    { className: "mainleaf__folder-icon"
    , name: isOpened ? "caret-down" $ "caret-right"
    , overlay: false
    , callback
    }

-----------------------------------------------

type NodeLinkProps =
  ( callback   :: Unit -> Effect Unit
  , href       :: String
  , id         :: Int
  , name       :: GT.Name
  , type       :: GT.NodeType
  )

nodeLink :: R2.Leaf NodeLinkProps
nodeLink = R2.leaf nodeLinkCpt
nodeLinkCpt :: R.Component NodeLinkProps
nodeLinkCpt = here.component "nodeLink" cpt where
  cpt { callback
      , href
      , id
      , name
      , type: nodeType
      } _ = do
    -- Computed
    let
      tid = tooltipId name id

      aProps =
        { href
        } `Record.merge` B.tooltipBind tid

    -- Render
    pure $

      H.div
      { className: "mainleaf__node-link"
      , on: { click: const $ callback unit }
      }
      [
        H.a
        aProps
        [
          B.span_ $ nodeLinkText nodeType name
        ]
      ]

nodeLinkText :: GT.NodeType -> String -> String
nodeLinkText GT.NodeUser s = s # (truncateNodeUser)
                         >>> maybe s identity
nodeLinkText _           s = textEllipsisBreak 15 s

truncateNodeUser :: String -> Maybe String
truncateNodeUser = Regex.match (nodeUserRegexp) >=> flip NArray.index 1 >>> join

---------------------------------------------------

type NodeTooltipProps =
  ( name      :: String
  , id        :: GT.NodeID
  , nodeType  :: GT.NodeType
  )

nodeTooltip :: R2.Component NodeTooltipProps
nodeTooltip = R2.component nodeTooltipCpt
nodeTooltipCpt :: R.Component NodeTooltipProps
nodeTooltipCpt = here.component "nodeTooltip" cpt where
  cpt { name, id, nodeType } children = pure $

    B.tooltip
    { id: tooltipId name id
    , effect: FloatEffect
    , delayShow: 600
    } $
    [
      H.b
      {}
      [
        B.icon
        { name: GT.getIcon nodeType true }
      ,
        B.span_ $
          GT.prettyNodeType nodeType
      ]
    ,
      B.div_ $
        name
    ]
      <> children

tooltipId :: String -> GT.NodeID -> String
tooltipId name id = name <> "-node-link-" <> show id

---------------------------------------------------------

blankNodeSpan :: R2.Leaf ()
blankNodeSpan = R2.leaf blankNodeSpanCpt
blankNodeSpanCpt :: R.Component ()
blankNodeSpanCpt = here.component "__blank__" cpt where
  cpt _ _ = pure $

    H.div { className: "mainleaf mainleaf--blank" }
    [
      B.icon { className: "mainleaf__folder-icon", name: "caret-right"}
    ,
      H.span { className: "mainleaf__node-icon" }
      [
        B.icon { name: "circle" }
      ]
    ,
      H.div { className: "mainleaf__node-link"} [ H.text $ nbsp 1 ]
    ]

-----------------------------------------------

-- START nodeActions

type NodeActionsCommon =
  ( id       :: ID
  , refresh  :: Unit -> Aff Unit
  , session  :: Session
  )

type NodeActionsProps = ( nodeType :: GT.NodeType | NodeActionsCommon )

nodeActions :: R2.Component NodeActionsProps
nodeActions = R.createElement nodeActionsCpt

nodeActionsCpt :: R.Component NodeActionsProps
nodeActionsCpt = here.component "nodeActions" cpt where
  cpt props _ = pure (child props.nodeType)
    where
      nodeActionsP      = SProxy :: SProxy "nodeType"

      childProps        = Record.delete nodeActionsP props

      child GT.NodeList = listNodeActions childProps
      child GT.Graph    = graphNodeActions childProps
      child _           = mempty

graphNodeActions :: R2.Leaf NodeActionsCommon
graphNodeActions = R2.leafComponent graphNodeActionsCpt
graphNodeActionsCpt :: R.Component NodeActionsCommon
graphNodeActionsCpt = here.component "graphNodeActions" cpt where
  cpt { id, session, refresh } _ = do
    -- States
    state /\ stateBox <- R2.useBox' Nothing

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: graphVersions session
      , path: id
      , state: stateBox
      }

    -- Render
    pure $ R2.fromMaybe state \gv ->

      nodeActionsGraph
      { graphVersions: gv, session, id, refresh }
      []

  graphVersions session graphId = GraphAPI.graphVersions { graphId, session }
  errorHandler = logRESTError here "[graphNodeActions]"


listNodeActions :: R2.Leaf NodeActionsCommon
listNodeActions = R2.leafComponent listNodeActionsCpt
listNodeActionsCpt :: R.Component NodeActionsCommon
listNodeActionsCpt = here.component "listNodeActions" cpt where
  cpt { id, session, refresh } _ = do
    -- States
    state /\ stateBox <- R2.useBox' Nothing

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: loadCorpusWithChild
      , path: { nodeId: id, session }
      , state: stateBox
      }

    -- Render
    pure $ R2.fromMaybe state \{ corpusId } ->

      nodeActionsNodeList
      { listId: id
      , nodeId: corpusId
      , session
      , refresh: refresh
      , nodeType: GT.TabNgramType GT.CTabTerms
      }

    where
      errorHandler = logRESTError here "[listNodeActions]"

-----------------------------------------------

type VersionComparatorProps =
  ( clientVersion :: Version
  , remoteVersion :: Version
  )

versionComparator :: R2.Leaf VersionComparatorProps
versionComparator = R2.leaf versionComparatorCpt
versionComparatorCpt :: R.Component VersionComparatorProps
versionComparatorCpt = here.component "versionComparator" cpt where
  cpt { clientVersion, remoteVersion } _
    | clientVersion == remoteVersion = pure $
        B.caveat
        { variant: Success
        , className: "mainleaf__version-comparator"
        }
        [
          B.b_ "Versions match"
        ,
          content clientVersion remoteVersion
        ]
    | otherwise = pure $
        B.caveat
        { variant: Warning
        , className: "mainleaf__version-comparator"
        }
        [
          B.b_ "Versions mismatch"
        ,
          content clientVersion remoteVersion
        ]

  content :: Version -> Version -> R.Element
  content clientVersion remoteVersion =
    H.ul
    {}
    [
      H.li
      {}
      [
        B.span_ "frontend: "
      ,
        H.text $ nbsp 1
      ,
        B.code_ clientVersion
      ]
    ,
      H.li
      {}
      [
        B.span_ "backend: "
      ,
        H.text $ nbsp 1
      ,
        B.code_ remoteVersion
      ]
    ]

-------------------------------------------------------

taskProgress :: R2.Leaf ()
taskProgress = R2.leaf taskProgressCpt
taskProgressCpt :: R.Component ()
taskProgressCpt = here.component "progress" cpt where
  cpt _ _ = do
    -- Context
    asyncProgressContext <- R.useContext asyncContext
    -- Render
    pure $

      H.span
      { className: "mainleaf__progress-bar" }
      [
        B.progressBar
        { value: asyncProgressContext
        , variant: Info
        }
      ]
