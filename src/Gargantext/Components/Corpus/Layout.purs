module Gargantext.Components.Corpus.Layout where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Sizing(..), Variant(..))
import Gargantext.Components.Corpus.EditionBlock (editionBlock)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.GraphQL.Node (Node)
import Gargantext.Components.TileMenu (tileMenu)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Session (useSession)
import Gargantext.Routes as GR
import Gargantext.Sessions (sessionId)
import Gargantext.Types (ID, defaultCacheParams)
import Gargantext.Utils (setter, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( nodeId      :: ID
  , nodeData    :: Node
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Corpus.Layout"

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt
layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
    cpt { nodeId, nodeData: { name } } _ = do
      -- | Hooks
      -- |
      boxes@{
        expandTableEdition
      } <- AppStore.use

      expandTableEdition' <- R2.useLive' expandTableEdition

      session <- useSession

      -- | Computed
      -- |
      let
        corpusCodeRoute = const do
          pure $ GR.CorpusCode (sessionId session) nodeId

      -- | Effect
      -- |

      -- transfer local Component change to Local Storage cache
      useFirstEffect' $
        flip T.listen expandTableEdition onExpandTableEditionChange


      -- | Behaviors
      -- |
      let
        onExpandClick _ = T.modify_ (not) expandTableEdition

      -- | Render
      -- |
      pure $

        H.div
        { className: "corpus-layout" }
        [
          -- FV.backButtonSmart { nodeId, session } []
          H.div
          { className: "corpus-layout__title" }
          [
            B.div'
            { className: "corpus-layout__title__text" }
            name
          ,
            H.hr
            { className: "corpus-layout__title__line"}
          ,
            B.iconButton
            { name: expandTableEdition' ?
                "caret-up" $
                "caret-down"
            , className: "corpos-layout__title__expand"
            , callback: onExpandClick
            }
          ]
        ,
          R2.when expandTableEdition' $

            H.div
            { className: "corpus-layout__edition-block" }
            [
              editionBlock
              { nodeId }
            ]
        ,
          H.div
          { className: "corpus-layout__code-section" }
          [
            tileMenu
            { boxes
            , currentTile: Just corpusCodeRoute
            , xTile: Just corpusCodeRoute
            , yTile: Just corpusCodeRoute
            }
            [
              B.button
              { callback: const $ pure unit
              , status: Muted
              , size: SmallSize
              , variant: ButtonVariant Secondary
              }
              [
                B.icon
                { name: "code" }
              ,
                B.wad_
                [ "d-inline-block", "virtual-space", "w-1" ]
              ,
                H.text "Code section"
              ]
            ]
          ]
        ,
          H.div
          { className: "corpus-layout__folders" }
          [
            FV.folderView
              { nodeId
              , session
              }
          ]
        ]

onExpandTableEditionChange :: T.Change Boolean -> Effect Unit
onExpandTableEditionChange { new } = do
  cache <- R2.loadLocalStorageState' R2.appParamsKey defaultCacheParams
  let update = setter (_ { expandTableEdition = new }) cache
  R2.setLocalStorageState R2.appParamsKey update
