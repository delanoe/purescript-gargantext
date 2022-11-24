module Gargantext.Components.Nodes.Corpus.Code where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as List
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Sizing(..), Variant(..))
import Gargantext.Components.Corpus.CodeSection (fieldsCodeEditor, loadCorpusWithReload, saveCorpus)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus.Types (Hyperdata(..))
import Gargantext.Components.Nodes.Types (FTFieldList(..), FTFieldsWithIndex(..), defaultField)
import Gargantext.Components.TileMenu (tileMenu)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Code"

type Props =
  ( nodeId          :: Int
  , session         :: Session
  , boxes           :: Boxes
  )

type ViewProps =
  ( corpus  :: NodePoly Hyperdata
  , nodeId  :: Int
  , reload  :: T2.ReloadS
  , session :: Session
  , boxes   :: Boxes
  )

corpusCodeLayout :: R2.Leaf Props
corpusCodeLayout = R2.leafComponent corpusCodeLayoutCpt
corpusCodeLayoutCpt :: R.Component Props
corpusCodeLayoutCpt = here.component "corpusCodeLayout" cpt where
  cpt { nodeId, session, boxes } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadCorpusWithReload
              , path: { nodeId, reload: reload', session }
              , render: \corpus -> corpusCodeView { corpus, nodeId, reload, session, boxes } }
    where
      errorHandler = logRESTError here "[corpusLayoutWithKey]"

corpusCodeView :: Record ViewProps -> R.Element
corpusCodeView props = R.createElement corpusCodeViewCpt props []
corpusCodeViewCpt :: R.Component ViewProps
corpusCodeViewCpt = here.component "corpusCodeView" cpt where
  cpt {corpus: (NodePoly {hyperdata: Hyperdata {fields: FTFieldList fields}}), nodeId, reload, session, boxes} _ = do
    let fieldsWithIndex = FTFieldsWithIndex $ mapWithIndex (\idx -> \ftField -> { idx, ftField }) fields
    fieldsS <- T.useBox fieldsWithIndex
    fields' <- T.useLive T.unequal fieldsS
    fieldsRef <- R.useRef fields

    -- handle props change of fields
    R.useEffect1' fields $ do
      if R.readRef fieldsRef == fields then
        pure unit
      else do
        R.setRef fieldsRef fields
        T.write_ fieldsWithIndex fieldsS

    corpusRoute <- pure $ const do
      pure $ GR.Corpus (sessionId session) nodeId

    pure $

      H.div
      { className: "corpus-code-layout" }
      [
        H.div
        { className: "corpus-code-layout__toolbar" }
        [
          tileMenu
          { boxes
          , currentTile: Just corpusRoute
          , xTile: Just corpusRoute
          , yTile: Just corpusRoute
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
              { name: "folder" }
            ,
              B.wad_
              [ "d-inline-block", "virtual-space", "w-1" ]
            ,
              H.text "Folders section"
            ]
          ]
        ,
          B.wad
          [ "d-flex", "justify-content-flex-end gap-1" ]
          [
            B.button
            { callback: onClickAdd fieldsS
            , variant: OutlinedButtonVariant Primary
            }
            [
              B.icon
              { name: "plus" }
            ,
              B.wad_
              [ "d-inline-block", "virtual-spacer", "w-1" ]
            ,
              H.text "New field"
            ]
          ,
            B.button
            { variant: ButtonVariant Primary
            , status: saveEnabled fieldsWithIndex fields'
            , callback: onClickSave
                          { fields: fields'
                          , nodeId
                          , reload
                          , session
                          }
            }
            [
              B.icon
              { name: "floppy-o" }
            ,
              B.wad_
              [ "d-inline-flex", "virtual-space", "w-1" ]
            ,
              H.text "Save changes"
            ]
          ]
        ]
      ,
        H.div
        { className: "corpus-code-layout__fields" }
        [
          fieldsCodeEditor
          { fields: fieldsS
          , nodeId
          , session
          } []
        ]
      ]

  saveEnabled :: FTFieldsWithIndex -> FTFieldsWithIndex -> ComponentStatus
  saveEnabled fs fsS = fs == fsS ? Disabled $ Enabled

  onClickSave :: forall e. { fields :: FTFieldsWithIndex
                            , nodeId :: Int
                            , reload :: T2.ReloadS
                            , session :: Session } -> e -> Effect Unit
  onClickSave {fields: FTFieldsWithIndex fields, nodeId, reload, session} _ = do
    launchAff_ do
      res <- saveCorpus $ { hyperdata: Hyperdata {fields: FTFieldList $ (_.ftField) <$> fields}
                          , nodeId
                          , session }
      liftEffect $ do
        _ <- case res of
              Left err -> here.warn2 "[corpusLayoutView] onClickSave RESTError" err
              _ -> pure unit
        T2.reload reload

  onClickAdd :: forall e. T.Box FTFieldsWithIndex -> e -> Effect Unit
  onClickAdd fieldsS _ = do
    T.modify_ (\(FTFieldsWithIndex fs) -> FTFieldsWithIndex $
      List.snoc fs $ { idx: List.length fs, ftField: defaultField }) fieldsS
