module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Data.Either (fromRight')
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype)
import Data.String.Regex as DSR
import Data.String.Regex.Flags as DSRF
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import React.SyntheticEvent     as E
import Reactix                  as R
import Reactix.DOM.HTML         as H
import Toestand as T
import URI.Extra.QueryPairs     as QP
-- import Web.File.Blob (Blob)
import Web.File.FileReader.Aff (readAsDataURL)

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Action(..), Props)
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileType(..), UploadFileBlob(..), readUFBAsText)
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, formChoiceSafe, panel)
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Routes       as GR
import Gargantext.Sessions (Session, postWwwUrlencoded, post)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types         as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Upload"

-- UploadFile Action

-- | Action : Upload
type ActionUpload =
  ( dispatch :: Action -> Aff Unit
  , id       :: ID
  , nodeType :: NodeType
  , session  :: Session )

actionUpload :: R2.Component ActionUpload
actionUpload = R.createElement actionUploadCpt
actionUploadCpt :: R.Component ActionUpload
actionUploadCpt = here.component "actionUpload" cpt where
  cpt { nodeType: Corpus, dispatch, id, session } _ = pure $ uploadFileView {dispatch, id, nodeType: GT.Corpus, session}
  cpt { nodeType: NodeList, dispatch, id, session } _ = pure $ uploadTermListView {dispatch, id, nodeType: GT.NodeList, session}
  cpt props@{ nodeType: _, dispatch, id, session } _ = pure $ actionUploadOther props []

{-
actionUpload Annuaire id session dispatch =
  pure $ uploadFileView {dispatch, id, nodeType: Annuaire, session}
  -}

actionUploadOther :: R2.Component ActionUpload
actionUploadOther = R.createElement actionUploadOtherCpt
actionUploadOtherCpt :: R.Component ActionUpload
actionUploadOtherCpt = here.component "actionUploadOther" cpt where
  cpt _ _ = do
    pure $ fragmentPT $ "Soon, upload for this NodeType."


-- file upload types
data DroppedFile =
  DroppedFile { blob     :: UploadFileBlob
              , fileType :: Maybe FileType
              , lang     :: Lang
              }
derive instance Generic DroppedFile _
instance Eq DroppedFile where
  eq = genericEq

type FileHash = String

type UploadFile =
  { blob :: UploadFileBlob
  , name :: String
  }
  

uploadFileView :: Record Props -> R.Element
uploadFileView props = R.createElement uploadFileViewCpt props []
uploadFileViewCpt :: R.Component Props
uploadFileViewCpt = here.component "uploadFileView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      -- mFile    :: R.State (Maybe UploadFile) <- R.useState' Nothing
      mFile <- T.useBox (Nothing :: Maybe UploadFile)
      fileType <- T.useBox CSV
      lang <- T.useBox EN

      let setFileType' val = T.write_ val fileType
      let setLang' val = T.write_ val lang

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
                                 ] CSV setFileType' show
                ]
              ]
            , R2.row
              [ H.div {className:"col-6 flex-space-around"}
                [ formChoiceSafe [EN, FR, No_extraction, Universal] EN setLang'
                  show
                ]
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

    onChangeContents :: forall e. T.Box (Maybe UploadFile) -> E.SyntheticEvent_ e -> Effect Unit
    onChangeContents mFile e = do
      let mF = R2.inputFileNameWithBlob 0 e
      E.preventDefault e
      E.stopPropagation e
      case mF of
        Nothing -> pure unit
        Just {blob, name} -> void $ launchAff do
          --contents <- readAsText blob
          --contents <- readAsDataURL blob
          liftEffect $ do
            T.write_ (Just $ {blob: UploadFileBlob blob, name}) mFile


type UploadButtonProps =
  ( dispatch :: Action -> Aff Unit
  , fileType :: T.Box FileType
  , id       :: GT.ID
  , lang     :: T.Box Lang
  , mFile    :: T.Box (Maybe UploadFile)
  , nodeType :: GT.NodeType
  )

uploadButton :: Record UploadButtonProps -> R.Element
uploadButton props = R.createElement uploadButtonCpt props []
uploadButtonCpt :: R.Component UploadButtonProps
uploadButtonCpt = here.component "uploadButton" cpt
  where
    cpt { dispatch
        , fileType
        , id
        , lang
        , mFile
        , nodeType
        } _ = do
      fileType' <- T.useLive T.unequal fileType
      mFile' <- T.useLive T.unequal mFile

      let disabled = case mFile' of
            Nothing -> "1"
            Just _  -> ""

      pure $ H.button { className: "btn btn-primary"
                      , "type" : "button"
                      , disabled
                      , style    : { width: "100%" }
                      , on: {click: onClick fileType' mFile'}
                      } [ H.text "Upload" ]
      where
        onClick fileType' mFile' e = do
          let { blob, name } = unsafePartial $ fromJust mFile'
          log2 "[uploadButton] fileType" fileType'
          void $ launchAff do
            case fileType' of
              Arbitrary ->
                dispatch $ UploadArbitraryFile (Just name) blob
              _ -> do
                contents <- readUFBAsText blob
                dispatch $ UploadFile nodeType fileType' (Just name) contents
            liftEffect $ do
              T.write_ Nothing mFile
              T.write_ CSV fileType
              T.write_ EN lang
            dispatch ClosePopover

-- START File Type View
type FileTypeProps =
  ( dispatch    :: Action -> Aff Unit
  , droppedFile :: T.Box (Maybe DroppedFile)
  , id          :: ID
  , isDragOver  :: T.Box Boolean
  , nodeType    :: GT.NodeType
  )

fileTypeView :: Record FileTypeProps -> R.Element
fileTypeView p = R.createElement fileTypeViewCpt p []
fileTypeViewCpt :: R.Component FileTypeProps
fileTypeViewCpt = here.component "fileTypeView" cpt
  where
    cpt { dispatch
        , droppedFile
        , isDragOver
        , nodeType
        } _ = do
      droppedFile' <- T.useLive T.unequal droppedFile

      case droppedFile' of
        Nothing -> pure $ H.div {} []
        Just df@(DroppedFile { blob, fileType }) ->
          pure $ H.div tooltipProps [ H.div { className: "card"}
                                      [ panelHeading
                                      , panelBody df
                                      , panelFooter df
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
                              T.write_ Nothing droppedFile
                              T.write_ false isDragOver
                          }
                    , title: "Close"} []
              ]
            ]
          ]

        panelBody (DroppedFile { blob }) =
          H.div {className: "card-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , on: {change: onChange blob}
                      }
                      (map renderOption [CSV, CSV_HAL, WOS])
          ]
          where
            onChange blob e l =
              T.write_ (Just $ DroppedFile $ { blob
                                             , fileType: read $ R.unsafeEventValue e
                                             , lang    : fromMaybe EN $ read $ R.unsafeEventValue l
                                             }) droppedFile
            renderOption opt = H.option {} [ H.text $ show opt ]

        panelFooter (DroppedFile { blob, fileType }) =
          H.div {className: "card-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , on: {click: \_ -> do
                                   T.write_ Nothing droppedFile
                                   launchAff $ do
                                     contents <- readUFBAsText blob
                                     dispatch $ UploadFile nodeType ft Nothing contents
                               }
                         } [H.text "Upload"]
              Nothing ->
                H.button {className: "btn btn-success disabled"
                         , type: "button"
                         } [H.text "Upload"]
          ]


newtype FileUploadQuery = FileUploadQuery {
    fileType :: FileType
  }
derive instance Newtype FileUploadQuery _
instance GT.ToQuery FileUploadQuery where
  toQuery (FileUploadQuery {fileType}) =
    QP.print id id $ QP.QueryPairs $
         pair "fileType" fileType
    where pair :: forall a. Show a => String -> a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k v = [ QP.keyFromString k /\ (Just $ QP.valueFromString $ show v) ]

uploadFile :: Session
           -> GT.NodeType
           -> ID
           -> FileType
           -> {contents :: String, mName :: Maybe String}
           -> Aff GT.AsyncTaskWithType
uploadFile session NodeList id fileType { mName, contents } = do
  let url = GR.NodeAPI NodeList (Just id) $ GT.asyncTaskTypePath GT.ListUpload
    -- { input: { data: ..., filetype: "JSON", name: "..." } }
  let body = { input: { data: contents
                      , filetype: "JSON"
                      , name: fromMaybe "" mName } }
  task <- post session url body
  pure $ GT.AsyncTaskWithType { task, typ: GT.Form }
uploadFile session nodeType id fileType { mName, contents } = do
  -- contents <- readAsText blob
  task <- postWwwUrlencoded session p bodyParams
  pure $ GT.AsyncTaskWithType {task, typ: GT.Form}
    --postMultipartFormData session p fileContents
  where
    p = case nodeType of
      Corpus   -> GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.Form
      Annuaire -> GR.NodeAPI nodeType (Just id) "annuaire"
      _        -> GR.NodeAPI nodeType (Just id) ""

    bodyParams = [ Tuple "_wf_data"     (Just contents)
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
    let re = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ DSR.regex "data:.*;base64," DSRF.noFlags
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
uploadTermListViewCpt = here.component "uploadTermListView" cpt
  where
    cpt {dispatch, id, nodeType} _ = do
      let defaultUploadType = JSON
      mFile <- T.useBox (Nothing :: Maybe UploadFile)
      uploadType <- T.useBox defaultUploadType

      let input = H.input { type: "file"
                          , placeholder: "Choose file"
                          , on: {change: onChangeContents mFile}
                          , className: "form-control"
                          }

      let opt fileType = H.option { value: show fileType } [ H.text $ show fileType ]
      
      let uploadTypeHtml = R2.select { className: "form-control"
                                     , defaultValue: show defaultUploadType
                                     , on: { change: onUploadTypeChange uploadType } } (opt <$> [ CSV, JSON ])

      let footer = H.div {} [ uploadTermButton { dispatch
                                               , id
                                               , mFile
                                               , nodeType
                                               , uploadType
                                               }
                            ]

      pure $ panel
        [ H.form {}
          [ R2.row [ R2.col 12 [ input ] ]
          , R2.row [ R2.col 12 [ uploadTypeHtml ] ]
        ]
      ] footer

    onChangeContents :: forall e. T.Box (Maybe UploadFile)
                     -> E.SyntheticEvent_ e
                     -> Effect Unit
    onChangeContents mFile e = do
      let mF = R2.inputFileNameWithBlob 0 e
      E.preventDefault  e
      E.stopPropagation e
      case mF of
        Nothing -> pure unit
        Just {blob, name} -> void $ launchAff do
          --contents <- readAsText blob
          liftEffect $ do
            T.write_ (Just $ { blob: UploadFileBlob blob
                             , name }) mFile

    onUploadTypeChange uploadType e = do
      case read (R.unsafeEventValue e) of
        Nothing -> pure unit
        Just fileType -> T.write_ fileType uploadType


type UploadTermButtonProps =
  ( dispatch :: Action -> Aff Unit
  , id       :: Int
  , mFile    :: T.Box (Maybe UploadFile)
  , nodeType :: GT.NodeType
  , uploadType :: T.Box FileType
  )

uploadTermButton :: R2.Leaf UploadTermButtonProps
uploadTermButton props = R.createElement uploadTermButtonCpt props []
uploadTermButtonCpt :: R.Component UploadTermButtonProps
uploadTermButtonCpt = here.component "uploadTermButton" cpt
  where
    cpt { dispatch
        , id
        , mFile
        , nodeType
        , uploadType } _ = do
      mFile' <- T.useLive T.unequal mFile
      uploadType' <- T.useLive T.unequal uploadType

      R.useEffect' $ do
        here.log2 "[uploadTermButton] uploadType'" uploadType'

      let disabled = case mFile' of
            Nothing -> "1"
            Just _  -> ""

      pure $ H.button { className: "btn btn-primary"
                      , disabled
                      , on: { click: onClick mFile' uploadType' }
                      } [ H.text "Upload" ]
      where
        onClick mFile' uploadType' e = do
          let {name, blob} = unsafePartial $ fromJust mFile'
          void $ launchAff do
            contents <- readUFBAsText blob
            _ <- dispatch $ UploadFile nodeType uploadType' (Just name) contents
            liftEffect $ do
              T.write_ Nothing mFile
