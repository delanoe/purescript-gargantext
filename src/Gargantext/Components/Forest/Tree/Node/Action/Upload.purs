module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), Props)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileContents(..))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, formChoiceSafe)
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Prelude (class Show, Unit, discard, bind, const, id, map, pure, show, unit, void, ($), read)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, postWwwUrlencoded)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import Web.File.FileReader.Aff (readAsText)

-- UploadFile Action

-- | Action : Upload
actionUpload :: NodeType -> ID -> Session -> (Action -> Aff Unit) -> R.Hooks R.Element
actionUpload NodeList id session dispatch =
  pure $ uploadTermListView {dispatch, id, nodeType: GT.NodeList, session}

actionUpload Corpus id session dispatch =
  pure $ uploadFileView {dispatch, id, nodeType: Corpus, session}

actionUpload _ _ _ _ =
  pure $ fragmentPT $ "Soon, upload for this NodeType."


-- file upload types
data DroppedFile =
  DroppedFile { contents :: UploadFileContents
              , fileType :: Maybe FileType
              , lang     :: Lang
              }

type FileHash = String


type UploadFile = 
  { contents :: UploadFileContents
  , name     :: String
  }


uploadFileView :: Record Props -> R.Element
uploadFileView props = R.createElement uploadFileViewCpt props []

uploadFileViewCpt :: R.Component Props
uploadFileViewCpt = R.hooksComponent "G.C.F.T.N.A.U.UploadFileView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      mFile    :: R.State (Maybe UploadFile) <- R.useState' Nothing
      fileType@(_ /\ setFileType)   <- R.useState'  CSV
      lang@( _chosenLang /\ setLang) <- R.useState' EN

      pure $
        H.div {className:""}
              [ H.div {className:"row"}
                      [ H.div {className:"col-md-6 flex-space-around"}
                              [ H.input { type: "file"
                                        , placeholder: "Choose file"
                                        , on: {change: onChangeContents mFile}
                                        }
                              ]
                      , H.div {className:"col-md-3 flex-space-around"}
                              [ formChoiceSafe [ CSV
                                               , CSV_HAL
                                               , WOS
                                               , PresseRIS
                                               ] CSV setFileType
                              ]

                      , H.div {className:"col-md-3 flex-space-around"}
                              [ formChoiceSafe [EN, FR, No_extraction, Universal] EN setLang ]
                      ]

              , H.div { className : "panel-footer" }
                      [ H.div {} []
                      , H.div {className:"flex-center"} 
                              [ uploadButton { dispatch
                                             , fileType
                                             , lang
                                             , id
                                             , mFile
                                             , nodeType
                                             }
                              ]
                      ]
              ]

    renderOptionFT :: FileType -> R.Element
    renderOptionFT opt = H.option {} [ H.text $ show opt ]

    renderOptionLang :: Lang -> R.Element
    renderOptionLang opt = H.option {} [ H.text $ show opt ]

    onChangeContents :: forall e. R.State (Maybe UploadFile) -> E.SyntheticEvent_ e -> Effect Unit
    onChangeContents (mFile /\ setMFile) e = do
      let mF = R2.inputFileNameWithBlob 0 e
      E.preventDefault e
      E.stopPropagation e
      case mF of
        Nothing -> pure unit
        Just {blob, name} -> void $ launchAff do
          contents <- readAsText blob
          liftEffect $ do
            setMFile $ const $ Just $ {contents: UploadFileContents contents, name}

    onChangeFileType :: forall e
                     .  R.State FileType
                     -> e
                     -> Effect Unit
    onChangeFileType (fileType /\ setFileType) e = do
      setFileType $ const
                  $ unsafePartial
                  $ fromJust
                  $ read
                  $ R2.unsafeEventValue e


type UploadButtonProps =
  ( dispatch :: Action -> Aff Unit
  , fileType :: R.State FileType
  , id       :: GT.ID
  , lang     :: R.State Lang
  , mFile    :: R.State (Maybe UploadFile)
  , nodeType :: GT.NodeType
  )

uploadButton :: Record UploadButtonProps -> R.Element
uploadButton props = R.createElement uploadButtonCpt props []

uploadButtonCpt :: R.Component UploadButtonProps
uploadButtonCpt = R.hooksComponent "G.C.F.T.N.A.U.uploadButton" cpt
  where
    cpt { dispatch
        , fileType: (fileType /\ setFileType)
        , id
        , lang: (lang /\ setLang)
        , mFile: (mFile /\ setMFile)
        , nodeType
        } _ = pure
            $ H.button { className: "btn btn-primary"
                       , "type" : "button"
                       , disabled
                       , style    : { width: "100%" }
                       , on: {click: onClick}
                       } [ H.text "Upload" ]
      where
        disabled = case mFile of
          Nothing -> "1"
          Just _ -> ""

        onClick e = do
          let {name, contents} = unsafePartial $ fromJust mFile
          void $ launchAff do
            _ <- dispatch $ UploadFile nodeType fileType (Just name) contents
            liftEffect $ do
              setMFile     $ const $ Nothing
              setFileType  $ const $ CSV
              setLang      $ const $ EN

-- START File Type View
type FileTypeProps =
  ( dispatch    :: Action -> Aff Unit
  , droppedFile :: R.State (Maybe DroppedFile)
  , id          :: ID
  , isDragOver  :: R.State Boolean
  , nodeType    :: GT.NodeType
  )

fileTypeView :: Record FileTypeProps -> R.Element
fileTypeView p = R.createElement fileTypeViewCpt p []

fileTypeViewCpt :: R.Component FileTypeProps
fileTypeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.fileTypeView" cpt
  where
    cpt { dispatch
        , droppedFile: Just (DroppedFile {contents, fileType}) /\ setDroppedFile
        , isDragOver: (_ /\ setIsDragOver)
        , nodeType
        } _ = pure
            $ H.div tooltipProps [ H.div { className: "panel panel-default"}
                                         [ panelHeading
                                         , panelBody
                                         , panelFooter
                                         ]
                                 ]
      where
        tooltipProps = { className: ""
                       , id       : "file-type-tooltip"
                       , title    : "Choose file type"
                       , data     : { toggle: "tooltip"
                                    , placement: "right"
                                    }
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
                              setIsDragOver  $ const false
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
                                                            , fileType: read $ R2.unsafeEventValue e
                                                            , lang    : fromMaybe EN $ read $ R2.unsafeEventValue l
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
                                   launchAff $ dispatch $ UploadFile nodeType ft Nothing contents
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

uploadFile :: Session
           -> GT.NodeType
           -> ID
           -> FileType
           -> {contents :: UploadFileContents, mName :: Maybe String}
           -> Aff GT.AsyncTaskWithType
uploadFile session nodeType id fileType {mName, contents: UploadFileContents contents} = do
    task <- postWwwUrlencoded session p bodyParams
    pure $ GT.AsyncTaskWithType {task, typ: GT.Form}
    --postMultipartFormData session p fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    --p = NodeAPI GT.Corpus (Just id) $ "add/file/async/nobody" <> Q.print (toQuery q)
    p = GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.Form
    bodyParams = [
        Tuple "_wf_data"     (Just contents)
      , Tuple "_wf_filetype" (Just $ show fileType)
      , Tuple "_wf_name"      mName
      ]

------------------------------------------------------------------------

uploadTermListView :: Record Props -> R.Element
uploadTermListView props = R.createElement uploadTermListViewCpt props []

uploadTermListViewCpt :: R.Component Props
uploadTermListViewCpt = R.hooksComponent "G.C.F.T.N.A.U.UploadTermListView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      mFile :: R.State (Maybe UploadFile) <- R.useState' Nothing

      pure $ H.div {} [
        H.div {} [ H.input { type: "file"
                            , placeholder: "Choose file"
                            , on: {change: onChangeContents mFile}
                            }
                  ]

      , H.div {} [ uploadTermButton { dispatch, id, mFile, nodeType } ]
      ]

    onChangeContents :: forall e. R.State (Maybe UploadFile)
                     -> E.SyntheticEvent_ e
                     -> Effect Unit
    onChangeContents (mFile /\ setMFile) e = do
      let mF = R2.inputFileNameWithBlob 0 e
      E.preventDefault  e
      E.stopPropagation e
      case mF of
        Nothing -> pure unit
        Just {blob, name} -> void $ launchAff do
          contents <- readAsText blob
          liftEffect $ do
            setMFile $ const $ Just $ { contents: UploadFileContents contents
                                      , name
                                      }


type UploadTermButtonProps =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , mFile    :: R.State (Maybe UploadFile)
  , nodeType :: GT.NodeType
  )

uploadTermButton :: Record UploadTermButtonProps -> R.Element
uploadTermButton props = R.createElement uploadTermButtonCpt props []

uploadTermButtonCpt :: R.Component UploadTermButtonProps
uploadTermButtonCpt = R.hooksComponent "G.C.F.T.N.A.U.uploadTermButton" cpt
  where
    cpt {dispatch, id, mFile: (mFile /\ setMFile), nodeType} _ = do
        pure $ H.button {className: "btn btn-primary", disabled, on: {click: onClick}} [ H.text "Upload" ]
      where
        disabled = case mFile of
          Nothing -> "1"
          Just _  -> ""

        onClick e = do
          let {name, contents} = unsafePartial $ fromJust mFile
          void $ launchAff do
            _ <- dispatch $ UploadFile nodeType CSV (Just name) contents
            liftEffect $ do
              setMFile $ const $ Nothing



