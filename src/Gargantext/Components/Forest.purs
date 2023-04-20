module Gargantext.Components.Forest
  ( forestLayout
  , Props
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Map (empty)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Position(..), TooltipPosition(..), Variant(..))
import Gargantext.Components.Forest.Tree (treeLoader)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Sessions (Session(..), unSessions)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest"

-- Shared by components here with Tree
type Props =
  ( boxes     :: Boxes
  , frontends :: Frontends
  )

forestLayout :: R2.Leaf Props
forestLayout = R2.leaf forestLayoutCpt

forestLayoutCpt :: R.Component Props
forestLayoutCpt = here.component "forest" cpt where
  cpt { boxes: boxes@{ handed
                     , reloadForest
                     , sessions
                     , pinnedTreeId }
      , frontends } _ = do
    -- TODO Fix this. I think tasks shouldn't be a Box but only a Reductor
    -- tasks'        <- GAT.useTasks reloadRoot reloadForest
    -- R.useEffect' $ do
    --   T.write_ (Just tasks') tasks
    handed'       <- T.useLive T.unequal handed
    sessions'     <- T.useLive T.unequal sessions
    pinnedTreeId' <- T.useLive T.unequal pinnedTreeId
    -- forestOpen'   <- T.useLive T.unequal forestOpen
    -- reloadRoot'   <- T.useLive T.unequal reloadRoot
    -- route'        <- T.useLive T.unequal route

    -- TODO If `reloadForest` is set, `reload` state should be updated
    -- TODO fix tasks ref
    pure $

      H.div
      { className: "forest-layout" }
      (A.cons (plus { boxes }) (trees handed' pinnedTreeId' sessions'))
    where
      trees handed' pinnedTreeId' sessions' = (tree handed' pinnedTreeId') <$> unSessions sessions'
      tree handed' pinnedTreeId' s@(Session { treeId }) =
        H.div
        { className: "forest-layout__tree" }
        [
          treeLoader
          { boxes
          , frontends
          , handed: handed'
          , reload: reloadForest
          , root: r
          , session: s
          , key: "tree-" <> (show treeId)
          }
        ]
        where
          r = fromMaybe treeId (Map.lookup (show s) pinnedTreeId')

type Plus = ( boxes :: Boxes )

plus :: R2.Leaf Plus
plus = R2.leaf plusCpt
plusCpt :: R.Component Plus
plusCpt = here.component "plus" cpt where
  cpt { boxes: { backend, showLogin, pinnedTreeId} } _ = do
    -- Hooks
    { goToRoute } <- useLinkHandler

    -- Behaviors
    let
      click _ = do
        -- NOTE Reset backend in case G.C.N.Home.homeLayout set that to (Just b)
        -- from current url
        _ <- T.write Nothing backend
        T.write_ true showLogin

    -- Render
    pure $
      H.div
      { className: "forest-layout__action" }
      [
        B.tooltipContainer
        { delayShow: 600
        , position: TooltipPosition Right
        , tooltipSlot:
            B.span_ "Back to home"
        , defaultSlot:
            B.button
            { className: "forest-layout__action__button"
            , callback: const $ goToRoute Home
            , variant: ButtonVariant Light
            }
            [
              B.icon { name: "home" }
            ]
        }
      ,
        B.tooltipContainer
        { delayShow: 600
        , position: TooltipPosition Right
        , tooltipSlot:
          B.span_ "Reset pins"
        , defaultSlot:
          B.button
          { className: "forest-layout__action__button"
          , callback: \_ -> T.write_ empty pinnedTreeId
          , variant: ButtonVariant Light
          }
          [
            B.icon { name: "thumb-tack-inclined-cancel" }
          ]
        }
      ,
        B.tooltipContainer
        { delayShow: 600
        , position: TooltipPosition Right
        , tooltipSlot:
            B.span_ "Add or remove connection to the server(s)"
        , defaultSlot:
            B.button
            { className: "forest-layout__action__button"
            , callback: click
            , variant: ButtonVariant Light
            }
            [
              B.icon
              { name: "universal-access" }
            ,
              B.wad_ [ "d-inline-block", "w-1" ]
            ,
              H.text $ "Log in/out"
            ]
        }
      ]
  --, H.div { "type": "", className: "fa fa-plus-circle fa-lg"} []
  --, H.div { "type": "", className: "fa fa-minus-circle fa-lg"} []
  -- TODO same as the one in the Login Modal (same CSS)
  -- [ H.i { className: "material-icons md-36"} [] ]


-- forestLayout :: R2.Leaf Props
-- forestLayout = R2.leaf forestLayoutCpt
-- forestLayoutCpt :: R.Memo Props
-- forestLayoutCpt = R.memo' $ here.component "forestLayout" cpt where
--   cpt p _ = pure $

--     H.div
--     { className: "forest-layout" }
--     [
--       H.div { className: "forest-layout__top-teaser" } []
--     ,
--       forest p []
--     ,
--       H.div { className: "forest-layout__bottom-teaser" } []
--     ]
