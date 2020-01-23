module Gargantext.Components.Nodes.Corpus where

import Data.Argonaut (class DecodeJson, encodeJson)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.CodeEditor as CE
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Nodes.Corpus.Types
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (NodeType(..), AffTableResult)
import Gargantext.Utils.Reactix as R2

type Props = (
    nodeId  :: Int
  , session :: Session
  )

corpusLayout :: Record Props -> R.Element
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = R.hooksComponent "G.C.N.C.corpusLayout" cpt
  where
    cpt props@{nodeId, session} _ =
      useLoader props loadCorpus' $
        \corpus -> corpusLayoutView {corpus, nodeId, session}

type ViewProps = (
    corpus  :: NodePoly CorpusHyperdata
  | Props
  )

corpusLayoutView :: Record ViewProps -> R.Element
corpusLayoutView props = R.createElement corpusLayoutViewCpt props []

corpusLayoutViewCpt :: R.Component ViewProps
corpusLayoutViewCpt = R.hooksComponent "G.C.N.C.corpusLayoutView" cpt
  where
    cpt {corpus: (NodePoly {hyperdata: CorpusHyperdata {fields}}), nodeId, session} _ = do
      fieldsS <- R.useState' fields

      pure $ H.div {} [
          H.div {} (corpusFieldCodeEditor {nodeId, session} <$> (fst fieldsS))
        , H.div { className: "row" } [
            H.div { className: "btn btn-default"
                  , on: { click: onClick fieldsS }
                  } [
              H.span { className: "glyphicon glyphicon-plus" } [  ]
            ]
          ]
        ]
          --H.iframe { src: gargMd , width: "100%", height: "100%", style: {"border-style": "none"}} []
    --gargMd = "https://hackmd.iscpif.fr/g9Aah4iwQtCayIzsKQjA0Q#"
    onClick (_ /\ setFieldsS) _ = do
      setFieldsS $ \fieldsS -> A.snoc fieldsS $ CorpusField {
          name: "New file"
        , typ: Markdown {
            tag: "MarkdownField"
          , text: "# New file"
          }
        }

corpusFieldCodeEditor :: Record LoadProps -> CorpusField CorpusFieldType -> R.Element
corpusFieldCodeEditor p (CorpusField {name, typ}) =
  H.div { className: "row panel panel-default" } [
      H.div { className: "panel-heading" } [ H.text name ]
    , H.div { className: "panel-body" } [
        corpusFieldCodeEditor' typ
      ]
    ]

corpusFieldCodeEditor' :: CorpusFieldType -> R.Element
corpusFieldCodeEditor' (Markdown {text}) =
  CE.codeEditor {code: text, defaultCodeType: CE.Markdown, onChange}
  where
    onChange c = do
      log2 "[corpusFieldCodeEditor'] Markdown c" c
corpusFieldCodeEditor' (JSON j) = do
  CE.codeEditor {code, defaultCodeType: CE.JSON, onChange}
  where
    code = R2.stringify (encodeJson j) 2
    onChange c = do
      log2 "[corpusFieldCodeEditor'] JSON c" c

type LoadProps = (
    nodeId  :: Int
  , session :: Session
  )

loadCorpus' :: Record LoadProps -> Aff (NodePoly CorpusHyperdata)
loadCorpus' {nodeId, session} = get session $ NodeAPI Corpus (Just nodeId) ""

loadCorpus :: Record LoadProps -> Aff CorpusData
loadCorpus {nodeId, session} = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session nodePolyRoute
  corpusNode     <- get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId) :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (A.head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just nodeId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just
