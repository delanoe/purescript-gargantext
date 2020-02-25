module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Data.Array as A
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Prelude

import Gargantext.Components.Data.Lang (readLang, Lang(..))
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), postWwwUrlencoded, get)
import Gargantext.Types as GT
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2

type Props =
  ( dispatch :: Action -> Aff Unit
  , id :: Int
  , nodeType :: GT.NodeType
  , session :: Session
  )


uploadFileView :: Record Props -> R.Element
uploadFileView props = R.createElement uploadFileViewCpt props []

uploadFileViewCpt :: R.Component Props
uploadFileViewCpt = R.hooksComponent "G.C.F.T.N.A.U.UploadFileView" cpt
  where
    cpt {dispatch: d, id, nodeType} _ = do
      mContents :: R.State (Maybe UploadFileContents) <- R.useState' Nothing
      fileType :: R.State FileType     <- R.useState' CSV
      lang     :: R.State (Maybe Lang) <- R.useState' (Just EN)

      pure $ H.div {} [
              H.div {} [ H.input { type: "file"
                                 , placeholder: "Choose file"
                                 , on: {change: onChangeContents mContents}
                                 }
                       ]

            , H.div {} [ R2.select {className: "col-md-12 form-control"
                                   , on: {change: onChangeFileType fileType}
                                   }
                          ( map renderOptionFT [ CSV
                                               , CSV_HAL
                                               , WOS
                                               , PresseRIS
                                               ]
                           )
                       ]


            , H.div {} [ R2.select {className: "col-md-12 form-control"
                       , on: {change: onChangeLang lang}
                       } (map renderOptionLang [EN, FR])
                       ]

            , H.div {} [ uploadButton {action: d, fileType, lang, id, mContents, nodeType } ]
            ]

    renderOptionFT :: FileType -> R.Element
    renderOptionFT opt = H.option {} [ H.text $ show opt ]

    renderOptionLang :: Lang -> R.Element
    renderOptionLang opt = H.option {} [ H.text $ show opt ]

    onChangeContents :: forall e. R.State (Maybe UploadFileContents) -> E.SyntheticEvent_ e -> Effect Unit
    onChangeContents (mContents /\ setMContents) e = do
      blob <- R2.inputFileBlob e
      E.preventDefault e
      E.stopPropagation e
      void $ launchAff do
        contents <- readAsText blob
        liftEffect $ do
          setMContents $ const $ Just $ UploadFileContents contents

    onChangeFileType :: forall e. R.State FileType -> e -> Effect Unit
    onChangeFileType (fileType /\ setFileType) e = do
      setFileType $ const
                  $ unsafePartial
                  $ fromJust
                  $ readFileType 
                  $ R2.unsafeEventValue e

    onChangeLang :: forall e. R.State (Maybe Lang) -> e -> Effect Unit
    onChangeLang (lang /\ setLang) e = do
      setLang $ const
              $ unsafePartial
              $ readLang
              $ R2.unsafeEventValue e


type UploadButtonProps =
  (
    action :: Action -> Aff Unit
  , fileType :: R.State FileType
  , id :: Int
  , lang :: R.State (Maybe Lang)
  , mContents :: R.State (Maybe UploadFileContents)
  , nodeType :: GT.NodeType
  )

uploadButton :: Record UploadButtonProps -> R.Element
uploadButton props = R.createElement uploadButtonCpt props []

uploadButtonCpt :: R.Component UploadButtonProps
uploadButtonCpt = R.hooksComponent "G.C.F.T.N.A.U.uploadButton" cpt
  where
    cpt {action, fileType: (fileType /\ setFileType), id, lang: (lang /\ setLang), mContents: (mContents /\ setMContents), nodeType} _ = do
        pure $ H.button {className: "btn btn-primary", disabled, on: {click: onClick}} [ H.text "Upload" ]
      where
        disabled = case mContents of
          Nothing -> "1"
          Just _ -> ""

        onClick e = do
          let contents = unsafePartial $ fromJust mContents
          void $ launchAff do
            _ <- action $ UploadFile nodeType fileType contents
            liftEffect $ do
              setMContents $ const $ Nothing
              setFileType  $ const $ CSV
              setLang      $ const $ Just EN

-- START File Type View
type FileTypeProps =
  ( action :: Action -> Aff Unit
  , droppedFile :: R.State (Maybe DroppedFile)
  , id :: ID
  , isDragOver :: R.State Boolean
  , nodeType :: GT.NodeType
  )

fileTypeView :: Record FileTypeProps -> R.Element
fileTypeView p = R.createElement fileTypeViewCpt p []

fileTypeViewCpt :: R.Component FileTypeProps
fileTypeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.fileTypeView" cpt
  where
    cpt {action, droppedFile: (Just (DroppedFile {contents, fileType}) /\ setDroppedFile), isDragOver: (_ /\ setIsDragOver), nodeType} _ = do
      pure $ H.div tooltipProps $
        [ H.div {className: "panel panel-default"}
          [ panelHeading
          , panelBody
          , panelFooter
          ]
        ]
      where
        tooltipProps = { className: ""
                       , id: "file-type-tooltip"
                       , title: "Choose file type"
                       , data: {toggle: "tooltip", placement: "right"}
                       }
        panelHeading =
          H.div {className: "panel-heading"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Choose file type"] ]
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn glyphitem glyphicon glyphicon-remove-circle"
                    , on: {click: \_ -> do
                              setDroppedFile $ const Nothing
                              setIsDragOver $ const false
                          }
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody =
          H.div {className: "panel-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , on: {change: onChange}
                      }
            (map renderOption [CSV, CSV_HAL, WOS])
          ]
          where
            onChange e l =
              setDroppedFile $ const $ Just $ DroppedFile $ { contents
                                                            , fileType: readFileType $ R2.unsafeEventValue e
                                                            , lang    : readLang     $ R2.unsafeEventValue l
                                                            }
        renderOption opt = H.option {} [ H.text $ show opt ]
        panelFooter =
          H.div {className: "panel-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , on: {click: \_ -> do
                                   setDroppedFile $ const Nothing
                                   launchAff $ action $ UploadFile nodeType ft contents
                               }
                         } [H.text "Upload"]
              Nothing ->
                H.button {className: "btn btn-success disabled"
                         , type: "button"
                         } [H.text "Upload"]
          ]

    cpt {droppedFile: (Nothing /\ _)} _ = do
      pure $ H.div {} []


newtype FileUploadQuery = FileUploadQuery {
    fileType :: FileType
  }
derive instance newtypeSearchQuery :: Newtype FileUploadQuery _
instance fileUploadQueryToQuery :: GT.ToQuery FileUploadQuery where
  toQuery (FileUploadQuery {fileType}) =
    QP.print id id $ QP.QueryPairs $
         pair "fileType" fileType
    where pair :: forall a. Show a => String -> a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k v = [ QP.keyFromString k /\ (Just $ QP.valueFromString $ show v) ]

uploadFile :: Session -> GT.NodeType -> ID -> FileType -> UploadFileContents -> Aff GT.AsyncTaskWithType
uploadFile session nodeType id fileType (UploadFileContents fileContents) = do
    task <- postWwwUrlencoded session p bodyParams
    pure $ GT.AsyncTaskWithType {task, typ: GT.Form}
    --postMultipartFormData session p fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    --p = NodeAPI GT.Corpus (Just id) $ "add/file/async/nobody" <> Q.print (toQuery q)
    p = GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.Form
    bodyParams = [
        Tuple "_wf_data" (Just fileContents)
      , Tuple "_wf_filetype" (Just $ show fileType)
      ]

uploadTermListView :: Record Props -> R.Element
uploadTermListView props = R.createElement uploadTermListViewCpt props []

uploadTermListViewCpt :: R.Component Props
uploadTermListViewCpt = R.hooksComponent "G.C.F.T.N.A.U.UploadTermListView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      mContents :: R.State (Maybe UploadFileContents) <- R.useState' Nothing

      pure $ H.div {} [
        H.div {} [ H.input { type: "file"
                            , placeholder: "Choose file"
                            , on: {change: onChangeContents mContents}
                            }
                  ]

      , H.div {} [ uploadTermButton { dispatch, id, mContents, nodeType } ]
      ]

    onChangeContents :: forall e. R.State (Maybe UploadFileContents) -> E.SyntheticEvent_ e -> Effect Unit
    onChangeContents (mContents /\ setMContents) e = do
      blob <- R2.inputFileBlob e
      E.preventDefault e
      E.stopPropagation e
      void $ launchAff do
        contents <- readAsText blob
        liftEffect $ do
          setMContents $ const $ Just $ UploadFileContents contents


type UploadTermButtonProps =
  (
    dispatch :: Action -> Aff Unit
  , id :: Int
  , mContents :: R.State (Maybe UploadFileContents)
  , nodeType :: GT.NodeType
  )

uploadTermButton :: Record UploadTermButtonProps -> R.Element
uploadTermButton props = R.createElement uploadTermButtonCpt props []

uploadTermButtonCpt :: R.Component UploadTermButtonProps
uploadTermButtonCpt = R.hooksComponent "G.C.F.T.N.A.U.uploadTermButton" cpt
  where
    cpt {dispatch, id, mContents: (mContents /\ setMContents), nodeType} _ = do
        pure $ H.button {className: "btn btn-primary", disabled, on: {click: onClick}} [ H.text "Upload" ]
      where
        disabled = case mContents of
          Nothing -> "1"
          Just _ -> ""

        onClick e = do
          let contents = unsafePartial $ fromJust mContents
          void $ launchAff do
            _ <- dispatch $ UploadFile nodeType CSV contents
            liftEffect $ do
              setMContents $ const $ Nothing

copyFromCorpusView :: Record Props -> R.Element
copyFromCorpusView props = R.createElement copyFromCorpusViewCpt props []

copyFromCorpusViewCpt :: R.Component Props
copyFromCorpusViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusView" cpt
  where
    cpt {dispatch, id, nodeType, session} _ = do
      useLoader session loadCorporaTree $
        \tree ->
          copyFromCorpusViewLoaded {dispatch, id, nodeType, session, tree}

type CorpusTreeProps =
  (
    tree :: FTree
    | Props
  )

copyFromCorpusViewLoaded :: Record CorpusTreeProps -> R.Element
copyFromCorpusViewLoaded props = R.createElement copyFromCorpusViewLoadedCpt props []

copyFromCorpusViewLoadedCpt :: R.Component CorpusTreeProps
copyFromCorpusViewLoadedCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusViewLoadedCpt" cpt
  where
    cpt p@{dispatch, id, nodeType, session, tree} _ = do
      pure $ H.div { className: "copy-from-corpus" } [
        H.div { className: "tree" } [copyFromCorpusTreeView p]
      ]

copyFromCorpusTreeView :: Record CorpusTreeProps -> R.Element
copyFromCorpusTreeView props = R.createElement copyFromCorpusTreeViewCpt props []

copyFromCorpusTreeViewCpt :: R.Component CorpusTreeProps
copyFromCorpusTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusTreeViewCpt" cpt
  where
    cpt p@{id, tree: NTree (LNode { id: sourceId, name, nodeType }) ary} _ = do
      pure $ {- H.div {} [ H.h5 { className: GT.fldr nodeType true} []
      , -} H.div { className: "node" } ([ H.span { className: "name " <> clickable
                                                              , on: { click: onClick }
                                                              } [ H.text name ]

                                                     ] <> children)
                      -- ]
      where
        children = map (\c -> copyFromCorpusTreeView (p { tree = c })) ary
        validNodeType = (A.elem nodeType [GT.NodeList]) && (id /= sourceId)
        clickable = if validNodeType then "clickable" else ""
        onClick _ = case validNodeType of
          false -> pure unit
          true  -> do
            log2 "[copyFromCorpusTreeViewCpt] issue copy into" id
            log2 "[copyFromCorpusTreeViewCpt] issue copy from" sourceId

loadCorporaTree :: Session -> Aff FTree
loadCorporaTree session = getCorporaTree session treeId
  where
    Session { treeId } = session

getCorporaTree :: Session -> Int -> Aff FTree
getCorporaTree session treeId = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" [ GT.FolderPrivate
                                                             , GT.FolderShared
                                                             , GT.Team
                                                             , GT.FolderPublic
                                                             , GT.Folder
                                                             , GT.Corpus
                                                             , GT.NodeList]
