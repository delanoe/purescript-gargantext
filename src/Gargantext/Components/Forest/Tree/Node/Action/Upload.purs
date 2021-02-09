module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Data.Either (fromRight)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.String.Regex as DSR
import Data.String.Regex.Flags as DSRF
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent     as E
import Reactix                  as R
import Reactix.DOM.HTML         as H
import URI.Extra.QueryPairs     as QP
-- import Web.File.Blob (Blob)
import Web.File.FileReader.Aff (readAsDataURL, readAsText)

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..), Props)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, formChoiceSafe, panel)
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Routes       as GR
import Gargantext.Sessions (Session, postWwwUrlencoded)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types         as GT
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Forest.Tree.Node.Action.Upload"

-- UploadFile Action

-- | Action : Upload
actionUpload :: NodeType -> ID -> Session -> (Action -> Aff Unit) -> R.Hooks R.Element
actionUpload NodeList id session dispatch =
  pure $ uploadTermListView {dispatch, id, nodeType: GT.NodeList, session}

actionUpload Corpus id session dispatch =
  pure $ uploadFileView {dispatch, id, nodeType: Corpus, session}

{-
actionUpload Annuaire id session dispatch =
  pure $ uploadFileView {dispatch, id, nodeType: Annuaire, session}
  -}

actionUpload _ _ _ _ =
  pure $ fragmentPT $ "Soon, upload for this NodeType."


-- file upload types
data DroppedFile =
  DroppedFile { blob :: UploadFileBlob
              , fileType :: Maybe FileType
              , lang     :: Lang
              }

type FileHash = String


type UploadFile = 
  { blob :: UploadFileBlob
  , name     :: String
  }


uploadFileView :: Record Props -> R.Element
uploadFileView props = R.createElement uploadFileViewCpt props []

uploadFileViewCpt :: R.Component Props
uploadFileViewCpt = R.hooksComponentWithModule thisModule "uploadFileView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      mFile    :: R.State (Maybe UploadFile) <- R.useState' Nothing
      fileType@(_ /\ setFileType)   <- R.useState'  CSV
      lang@( _chosenLang /\ setLang) <- R.useState' EN

      let setFileType' = setFileType <<< const
      let setLang' = setLang <<< const

      let bodies =
            [ R2.row
              [ H.div { className:"col-12 flex-space-around"}
                [ H.div { className: "form-group" }
                  [ H.input { type: "file"
                            , className: "form-control"
                            , placeholder: "Choose file"
                            , on: {change: onChangeContents mFile}
                            }
                  ]
                ]
              ]
            , R2.row
              [ H.div {className:"col-6 flex-space-around"}
                [ formChoiceSafe [ CSV
                                 , CSV_HAL
                                 , WOS
                                 , PresseRIS
                                 , Arbitrary
                                 ] CSV setFileType'
                ]
              ]
            , R2.row
              [ H.div {className:"col-6 flex-space-around"}
                [ formChoiceSafe [EN, FR, No_extraction, Universal] EN setLang' ]
              ]
            ]

      let footer = H.div {} [ uploadButton { dispatch
                                           , fileType
                                           , lang
                                           , id
                                           , mFile
                                           , nodeType
                                           }
                            ]
      pure $ panel bodies footer

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
          --contents <- readAsText blob
          --contents <- readAsDataURL blob
          liftEffect $ do
            setMFile $ const $ Just $ {blob: UploadFileBlob blob, name}


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
uploadButtonCpt = R.hooksComponentWithModule thisModule "uploadButton" cpt
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
          Just _  -> ""

        onClick e = do
          let { blob, name } = unsafePartial $ fromJust mFile
          log2 "[uploadButton] fileType" fileType
          void $ launchAff do
            case fileType of
              Arbitrary ->
                dispatch $ UploadArbitraryFile (Just name) blob
              _ ->
                dispatch $ UploadFile nodeType fileType (Just name) blob
            liftEffect $ do
              setMFile     $ const $ Nothing
              setFileType  $ const $ CSV
              setLang      $ const $ EN
            dispatch ClosePopover

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
fileTypeViewCpt = R.hooksComponentWithModule thisModule "fileTypeView" cpt
  where
    cpt { dispatch
        , droppedFile: Just (DroppedFile {blob, fileType}) /\ setDroppedFile
        , isDragOver: (_ /\ setIsDragOver)
        , nodeType
        } _ = pure
            $ H.div tooltipProps [ H.div { className: "card"}
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
          H.div {className: "card-header"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.h5 {} [H.text "Choose file type"] ]
            , H.div {className: "col-md-2"}
              [ H.a {className: "btn glyphitem fa fa-remove-circle"
                    , on: {click: \_ -> do
                              setDroppedFile $ const Nothing
                              setIsDragOver  $ const false
                          }
                    , title: "Close"} []
              ]
            ]
          ]

        panelBody =
          H.div {className: "card-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , on: {change: onChange}
                      }
                      (map renderOption [CSV, CSV_HAL, WOS])
          ]
          where
            onChange e l =
              setDroppedFile $ const $ Just $ DroppedFile $ { blob
                                                            , fileType: read $ R.unsafeEventValue e
                                                            , lang    : fromMaybe EN $ read $ R.unsafeEventValue l
                                                            }
            renderOption opt = H.option {} [ H.text $ show opt ]

        panelFooter =
          H.div {className: "card-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , on: {click: \_ -> do
                                   setDroppedFile $ const Nothing
                                   launchAff $ dispatch $ UploadFile nodeType ft Nothing blob
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
           -> {blob :: UploadFileBlob, mName :: Maybe String}
           -> Aff GT.AsyncTaskWithType
uploadFile session nodeType id fileType {mName, blob: UploadFileBlob blob} = do
  contents <- readAsText blob
  task <- postWwwUrlencoded session p (bodyParams contents)
  pure $ GT.AsyncTaskWithType {task, typ: GT.Form}
    --postMultipartFormData session p fileContents
  where
    p = case nodeType of
      Corpus   -> GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.Form
      Annuaire -> GR.NodeAPI nodeType (Just id) "annuaire"
      _        -> GR.NodeAPI nodeType (Just id) ""
      
    bodyParams c = [ Tuple "_wf_data"     (Just c)
                   , Tuple "_wf_filetype" (Just $ show fileType)
                   , Tuple "_wf_name"      mName
                   ]


uploadArbitraryFile :: Session
                    -> ID
                    -> {blob :: UploadFileBlob, mName :: Maybe String}
                    -> Aff GT.AsyncTaskWithType
uploadArbitraryFile session id {mName, blob: UploadFileBlob blob} = do
    contents <- readAsDataURL blob
    uploadArbitraryDataURL session id mName contents

uploadArbitraryDataURL :: Session
                       -> ID
                       -> Maybe String
                       -> String
                       -> Aff GT.AsyncTaskWithType
uploadArbitraryDataURL session id mName contents' = do
    let re = unsafePartial $ fromRight $ DSR.regex "data:.*;base64," DSRF.noFlags
        contents = DSR.replace re "" contents'
    task <- postWwwUrlencoded session p (bodyParams contents)
    pure $ GT.AsyncTaskWithType { task, typ: GT.Form }
  where
    p = GR.NodeAPI GT.Node (Just id) $ GT.asyncTaskTypePath GT.UploadFile

    bodyParams c = [ Tuple "_wfi_b64_data"  (Just c)
                   , Tuple "_wfi_name"      mName
                   ]

------------------------------------------------------------------------

uploadTermListView :: Record Props -> R.Element
uploadTermListView props = R.createElement uploadTermListViewCpt props []

uploadTermListViewCpt :: R.Component Props
uploadTermListViewCpt = R.hooksComponentWithModule thisModule "uploadTermListView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      mFile :: R.State (Maybe UploadFile) <- R.useState' Nothing
      let body = H.input { type: "file"
                            , placeholder: "Choose file"
                            , on: {change: onChangeContents mFile}
                            }

      let footer = H.div {} [ uploadTermButton { dispatch
                                               , id
                                               , mFile
                                               , nodeType
                                               } 
                            ]

      pure $ panel [body] footer

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
          --contents <- readAsText blob
          liftEffect $ do
            setMFile $ const $ Just $ { blob: UploadFileBlob blob
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
uploadTermButtonCpt = R.hooksComponentWithModule thisModule "uploadTermButton" cpt
  where
    cpt {dispatch, id, mFile: (mFile /\ setMFile), nodeType} _ = do
        pure $ H.button {className: "btn btn-primary", disabled, on: {click: onClick}} [ H.text "Upload" ]
      where
        disabled = case mFile of
          Nothing -> "1"
          Just _  -> ""

        onClick e = do
          let {name, blob} = unsafePartial $ fromJust mFile
          void $ launchAff do
            _ <- dispatch $ UploadFile nodeType CSV (Just name) blob
            liftEffect $ do
              setMFile $ const $ Nothing



