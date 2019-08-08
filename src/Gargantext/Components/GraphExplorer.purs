module Gargantext.Components.GraphExplorer where

import Effect.Unsafe (unsafePerformEffect)
import Gargantext.Prelude hiding (max,min)

import Control.Monad.Cont.Trans (lift)
import Data.Array (fold, length, (!!), null)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Lens (Lens', over, (%~), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Number as Num
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Hooks.Sigmax.Types as Sigmax
import Gargantext.Hooks.Sigmax.Sigmajs (CameraProps, SigmaNode, cameras, getCameraProps, goTo, pauseForceAtlas2, sigmaOnMouseMove)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Components.GraphExplorer.Legend (legend)
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.Graph as Graph
import Gargantext.Components.Login.Types (AuthData(..), TreeId)
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Tree as Tree
import Gargantext.Config as Config
import Gargantext.Config.REST (get)
import Gargantext.Pages.Corpus.Graph.Tabs as GT
import Gargantext.Router (Routes(..))
import Gargantext.Types (class Optional)
import Gargantext.Utils (toggleSet)
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)
import Reactix as R
import Reactix.DOM.HTML as RH

type Props = (
    mCurrentRoute :: Maybe Routes
  , treeId :: Maybe Int
)

spec :: Spec (Record GET.StateGlue) (Record Props) GET.Action
spec = simpleSpec GET.performAction render
 where
   render :: Render (Record GET.StateGlue) (Record Props) GET.Action
   render dispatch props state _ =
     [ R2.scuff $ specCpt dispatch state props ]

specCpt :: (GET.Action -> Effect Unit) -> Record GET.StateGlue -> Record Props -> R.Element
specCpt d stateGlue props = R.createElement el props []
  where
    el = R.hooksComponent "SpecCpt" cpt
    cpt props _children = do
      state <- GET.fromStateGlue stateGlue

      pure $ explorer state props

explorer :: Record GET.State -> Record Props -> R.Element
explorer state props = R.createElement (explorerCpt state) props []

--explorerCpt :: GET.State -> R.Component Props
explorerCpt state = R.hooksComponent "GraphExplorer" cpt
  where
    cpt {mCurrentRoute, treeId} _ = do
      controls <- Controls.useGraphControls
      pure $
        row
        [
          outer
          [ inner
            [ row1
              [ col [ pullLeft [ Toggle.treeToggleButton controls.showTree ] ]
              , col [ Toggle.controlsToggleButton controls.showControls ]
              , col [ pullRight [ Toggle.sidebarToggleButton controls.showSidePanel ] ]
              ]
            , row [ Controls.controls controls ]
            , row [ tree {mCurrentRoute, treeId} controls, graph controls, sidebar controls ]
            , row [ ]
            ]
          ]
        ]
    outer = RH.div { className: "col-md-9" }
    inner = RH.div { className: "container-fluid", style: { paddingTop: "90px" } }
    row1 = RH.div { className: "row", style: { paddingBottom: "10px", marginTop: "-24px" } }
    row = RH.div { className: "row" }
    col = RH.div { className: "col-md-4" }
    pullLeft = RH.div { className: "pull-left" }
    pullRight = RH.div { className: "pull-right" }

    tree {treeId: Nothing} _ = RH.div {} []
    tree _ {showTree: false /\ _} = RH.div {} []
    tree {mCurrentRoute, treeId: Just treeId} _ = RH.div {} [ Tree.elTreeview {mCurrentRoute, root: treeId} ]
    graph _ = RH.div {} []
    sidebar _ = RH.div {} []

convert :: GET.GraphData -> Graph.Graph
convert (GET.GraphData r) = Sigmax.Graph {nodes, edges}
  where
    nodes = foldMapWithIndex nodeFn r.nodes
    nodeFn i (GET.Node n) =
      Seq.singleton
        { id    : n.id_
        , size  : toNumber n.size
        , label : n.label
        , x     : n.x -- cos (toNumber i)
        , y     : n.y -- sin (toNumber i)
        , color : intColor (cDef n.attributes)
        }
      where
        cDef (GET.Cluster {clustDefault}) = clustDefault
    edges = foldMap edgeFn r.edges
    edgeFn (GET.Edge e) = Seq.singleton {id : e.id_, source : e.source, target : e.target}

defaultPalette :: Array String
defaultPalette = ["#5fa571","#ab9ba2","#da876d","#bdd3ff","#b399df","#ffdfed","#33c8f3","#739e9a","#caeca3","#f6f7e5","#f9bcca","#ccb069","#c9ffde","#c58683","#6c9eb0","#ffd3cf","#ccffc7","#52a1b0","#d2ecff","#99fffe","#9295ae","#5ea38b","#fff0b3","#d99e68"]

-- clusterColor :: Cluster -> Color
-- clusterColor (Cluster {clustDefault}) = unsafePartial $ fromJust $ defaultPalette !! (clustDefault `mod` length defaultPalette)


intColor :: Int -> String
intColor i = unsafePartial $ fromJust $ defaultPalette !! (i `mod` length defaultPalette)





getNodes :: Int -> Aff GET.GraphData
getNodes graphId = get $ Config.toUrl Config.Back Config.Graph $ Just graphId

getAuthData :: Effect (Maybe AuthData)
getAuthData = do
  w  <- window
  ls <- localStorage w
  mto <- getItem "token" ls
  mti <- getItem "tree_id" ls
  pure do
    token <- mto
    tree_id <- Int.fromString =<< mti
    pure $ AuthData {token, tree_id}

