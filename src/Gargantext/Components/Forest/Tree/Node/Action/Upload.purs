module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Gargantext.Prelude

import Affjax.RequestBody (blob)
import Data.Array (singleton)
import Data.Either (Either, fromRight')
import Data.Eq.Generic (genericEq)
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Data.String.Regex as DSR
import Data.String.Regex.Flags as DSRF
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.Forest.Tree.Node.Action (Props)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Upload.Types (FileFormat(..), FileType(..), UploadFileBlob(..), readUFBAsBase64, readUFBAsText)
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, formChoiceSafe, panel)
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.ListSelection as ListSelection
import Gargantext.Components.ListSelection.Types (Selection(..))
import Gargantext.Components.ListSelection.Types as ListSelection
import Gargantext.Config.REST (AffRESTError, RESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, postWwwUrlencoded, post)
import Gargantext.Types (ID, NodeType(..))
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Partial.Unsafe (unsafePartial, unsafeCrashWith)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import URI.Extra.QueryPairs as QP
import Web.File.FileReader.Aff (readAsDataURL)

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
  cpt { nodeType: Corpus, dispatch, id, session } _ =
    pure $ uploadFileView { dispatch, id, nodeType: GT.Corpus, session }

  cpt { nodeType: NodeList, dispatch, id, session } _ =
    pure $ uploadTermListView { dispatch, id, nodeType: GT.NodeList, session } []

  cpt props@{ nodeType: NodeFrameCalc } _ = pure $ uploadFrameCalcView props []

  cpt props@{ nodeType: Annuaire, dispatch, id, session } _ =
    pure $ uploadListView { dispatch, id, nodeType: GT.Annuaire, session }

  cpt props@{ nodeType: _ } _ = pure $ actionUploadOther props []

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
instance Eq DroppedFile where eq = genericEq

type FileHash = String

type UploadFile =
  { blob :: UploadFileBlob
  , name :: String
  }


uploadFileView :: R2.Leaf Props
uploadFileView = R2.leafComponent uploadFileViewCpt
uploadFileViewCpt :: R.Component Props
uploadFileViewCpt = here.component "uploadFileView" cpt
  where
    cpt { dispatch, id, nodeType, session } _ = do
      -- mFile    :: R.State (Maybe UploadFile) <- R.useState' Nothing
      mFile       <- T.useBox (Nothing :: Maybe UploadFile)
      fileType    <- T.useBox CSV
      fileFormat  <- T.useBox Plain
      lang        <- T.useBox EN
      selection   <- T.useBox ListSelection.MyListsFirst

      let setFileType'   val = T.write_ val fileType
      let setFileFormat' val = T.write_ val fileFormat
      let setLang'       val = T.write_ val lang

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
                [ formChoiceSafe { items: [ CSV
                                          , CSV_HAL
                                          , WOS
                                          , PresseRIS
                                          , Arbitrary
                                          ]
                                 , default: CSV
                                 , callback: setFileType'
                                 , print: show } []
                , formChoiceSafe { items: [ Plain
                                          , ZIP ]
                                 , default: Plain
                                 , callback: setFileFormat'
                                 , print: show } []
                ]
              ]
            , R2.row
              [ H.div {className:"col-6 flex-space-around"}
                [ formChoiceSafe { items: [EN, FR, No_extraction, Universal]
                                 , default: EN
                                 , callback: setLang'
                                 , print: show
                                 } []
                ]
              ]
            , R2.row
              [ H.div { className: "col-6 flex-space-around" }
                [ ListSelection.selection { selection, session } [] ]
              ]
            ]

      let footer = H.div {} [ uploadButton { dispatch
                                           , fileFormat
                                           , fileType
                                           , lang
                                           , mFile
                                           , nodeType
                                           , selection
                                           } []
                            ]
      pure $ panel bodies footer


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
  ( dispatch   :: Action -> Aff Unit
  , fileFormat :: T.Box FileFormat
  , fileType   :: T.Box FileType
  , lang       :: T.Box Lang
  , mFile      :: T.Box (Maybe UploadFile)
  , nodeType   :: GT.NodeType
  , selection  :: T.Box ListSelection.Selection
  )

uploadButton :: R2.Component UploadButtonProps
uploadButton = R.createElement uploadButtonCpt

uploadButtonCpt :: R.Component UploadButtonProps
uploadButtonCpt = here.component "uploadButton" cpt
  where
    cpt { dispatch
        , fileFormat
        , fileType
        , lang
        , mFile
        , nodeType
        , selection
        } _ = do
      fileType'   <- T.useLive T.unequal fileType
      fileFormat' <- T.useLive T.unequal fileFormat
      mFile'      <- T.useLive T.unequal mFile
      lang'       <- T.useLive T.unequal lang
      selection'  <- T.useLive T.unequal selection
      onPending /\ onPendingBox <- R2.useBox' false

      let disabled = isNothing mFile' || onPending

      pure $
        H.div
        { className: "action-upload-button" }
        [
          if onPending
          then H.div
               { className: intercalate " "
                 [ "spinner-grow"
                 , "action-upload-button__spinner"
                 ]
               }
               []
          else mempty
        ,
          H.button
          { className: "btn btn-primary"
          , "type" : "button"
          , disabled
          , style    : { width: "100%" }
          , on: { click: onClick
                          fileFormat'
                          fileType'
                          mFile'
                          lang'
                          selection'
                          onPendingBox
                }
          }
          [ H.text "Upload" ]
        ]


      where
        onClick fileFormat' fileType' mFile' lang' selection' onPendingBox e = do
          let { blob, name } = unsafePartial $ fromJust mFile'
          T.write_ true onPendingBox
          here.log2 "[uploadButton] fileType" fileType'
          void $ launchAff do
            case fileType' of
              Arbitrary ->
                dispatch $ UploadArbitraryFile fileFormat' (Just name) blob selection'
              _ -> do
                contents <- case fileFormat' of
                  Plain -> readUFBAsText blob
                  ZIP   -> readUFBAsBase64 blob
                dispatch $ UploadFile nodeType fileType' fileFormat' lang' (Just name) contents selection'
            liftEffect $ do
              T.write_ Nothing mFile
              T.write_ CSV fileType
              T.write_ Plain fileFormat
              T.write_ EN lang
              T.write_ false onPendingBox
            dispatch CloseBox

uploadListView :: R2.Leaf Props
uploadListView = R2.leafComponent uploadListViewCpt
uploadListViewCpt :: R.Component Props
uploadListViewCpt = here.component "uploadListView" cpt where
  cpt { dispatch, session } _ = do
  -- States
    mFile
      <- T.useBox (Nothing :: Maybe UploadFile)
    fileType
      <- T.useBox CSV
    fileFormat /\ fileFormatBox
      <- R2.useBox' Plain
    lang /\ langBox
      <- R2.useBox' EN
    selection
      <- T.useBox ListSelection.MyListsFirst

  -- Render
    pure $

      panel
      -- Body
      [
        -- Upload
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
              "Upload file"
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            H.input
            { type: "file"
            , className: "form-control"
            , placeholder: "Choose file"
            , on: { change: onChangeContents mFile }
            }
          ]
        ]
      ,
        -- Format
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
              "File format"
          ]
        ,
          H.div
          { className: "form-group__field d-flex justify-content-between" }
          [
            B.formSelect
            { callback: \_ -> pure unit
            , value: show CSV
            , status: Disabled
            , className: "col-5"
            }
            [
              H.option
              { value: show CSV }
              [ H.text $ show CSV ]
            ]
          ,
            B.formSelect'
            { callback: flip T.write_ fileFormatBox
            , value: fileFormat
            , list: [ Plain, ZIP ]
            , className: "col-5"
            }
            []
          ]
        ]
      ,
        -- Lang
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
              "File lang"
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formSelect'
            { callback: flip T.write_ langBox
            , value: lang
            , list: [ EN, FR, No_extraction, Universal ]
            }
            []
          ]
        ]
      ,
        -- List selection
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
              "List selection"
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            ListSelection.selection
            { selection
            , session
            } []
          ]
        ]
      ]
      -- Footer
      (
        H.div
        {}
        [
          uploadButton
          { dispatch
          , fileFormat: fileFormatBox
          , fileType
          , lang: langBox
          , mFile
          , selection
          , nodeType: GT.Annuaire
          } []
        ]
      )

-- START File Type View
type FileTypeProps =
  ( dispatch    :: Action -> Aff Unit
  , droppedFile :: T.Box (Maybe DroppedFile)
  , id          :: ID
  , isDragOver  :: T.Box Boolean
  , nodeType    :: GT.NodeType
  , key         :: String
  )

fileTypeView :: R2.Leaf FileTypeProps
fileTypeView = R2.leaf fileTypeViewCpt
fileTypeViewCpt :: R.Component FileTypeProps
fileTypeViewCpt = here.component "fileTypeView" cpt
  where
    cpt { dispatch
        , droppedFile
        , isDragOver
        , nodeType
        } _ = do
      droppedFile' <- T.useLive T.unequal droppedFile

      pure $

        R2.fromMaybe droppedFile' \df ->

          H.div
          tooltipProps
          [
            H.div { className: "card"}
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
                      , on: {change: onChange}
                      }
                      (map renderOption [CSV, CSV_HAL, WOS])
          ]
          where
            onChange e l =
              T.write_ (Just $ DroppedFile $ { blob
                                             , fileType: read $ R.unsafeEventValue e
                                             , lang    : fromMaybe EN $ read $ R.unsafeEventValue l
                                             }) droppedFile
            renderOption opt = H.option {} [ H.text $ show opt ]

        panelFooter (DroppedFile { blob, fileType, lang}) =
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
                                     dispatch $ UploadFile nodeType ft Plain lang Nothing contents (SelectedLists [])
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

uploadFile :: { contents  :: String
              , fileFormat :: FileFormat
              , fileType  :: FileType
              , id        :: ID
              , lang      :: Lang
              , nodeType  :: GT.NodeType
              , mName     :: Maybe String
              , selection :: ListSelection.Selection
              , session   :: Session }
           -> AffRESTError GT.AsyncTaskWithType
{-
uploadFile session NodeList id JSON { mName, contents } = do
  let url = GR.NodeAPI NodeList (Just id) $ GT.asyncTaskTypePath GT.ListUpload
    -- { input: { data: ..., filetype: "JSON", name: "..." } }
  let body = { input: { data: contents
                      , filetype: "JSON"
                      , name: fromMaybe "" mName } }
  task <- post session url body
  pure $ GT.AsyncTaskWithType { task, typ: GT.Form }
  -}
uploadFile { contents, fileFormat, lang, fileType, id, nodeType, mName, selection, session } = do
  -- contents <- readAsText blob
  eTask :: Either RESTError GT.AsyncTask <- postWwwUrlencoded session p body
  pure $ (\task -> GT.AsyncTaskWithType { task, typ }) <$> eTask
    --postMultipartFormData session p fileContents
  where
    bodyParams = [ Tuple "_wf_data"       (Just contents)
                 , Tuple "_wf_filetype"   (Just $ show fileType)
                 , Tuple "_wf_fileformat" (Just $ show fileFormat)
                 , Tuple "_wf_lang"       (Just $ show lang)
                 , Tuple "_wf_name"       mName
                 , Tuple "_wf_selection"  (Just $ show selection)
                 ]
    csvBodyParams = [ Tuple "_wtf_data"       (Just contents)
                    , Tuple "_wtf_filetype"   (Just $ show NodeList)
                    , Tuple "_wtf_fileformat" (Just $ show fileFormat)
                    , Tuple "_wf_lang"        (Just $ show lang)
                    , Tuple "_wtf_name" mName
                    , Tuple "_wtf_selection"  (Just $ show selection)
                    ]

    (typ /\ p /\ body) = case nodeType of
      Corpus   -> GT.CorpusFormUpload /\ (GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.CorpusFormUpload) /\ bodyParams
      Annuaire -> GT.UploadFile /\ (GR.NodeAPI nodeType (Just id) "annuaire") /\ bodyParams
      NodeList -> case fileType of
        JSON -> GT.ListUpload /\ (GR.NodeAPI nodeType (Just id) $ GT.asyncTaskTypePath GT.ListUpload) /\ bodyParams
        CSV  -> GT.ListCSVUpload /\ (GR.NodeAPI NodeList (Just id) $ GT.asyncTaskTypePath GT.ListCSVUpload) /\ csvBodyParams
        _    -> GT.UploadFile /\ (GR.NodeAPI nodeType (Just id) "") /\ bodyParams
      _        -> GT.UploadFile /\ (GR.NodeAPI nodeType (Just id) "") /\ bodyParams


uploadArbitraryFile :: Session
                    -> ID
                    -> {blob :: UploadFileBlob, fileFormat :: FileFormat, mName :: Maybe String}
                    -> ListSelection.Selection
                    -> AffRESTError GT.AsyncTaskWithType
uploadArbitraryFile session id { fileFormat, mName, blob: UploadFileBlob blob } selection = do
    contents <- readAsDataURL blob
    uploadArbitraryData session id fileFormat mName contents

uploadArbitraryData :: Session
                    -> ID
                    -> FileFormat
                    -> Maybe String
                    -> String
                    -> AffRESTError GT.AsyncTaskWithType
uploadArbitraryData session id fileFormat mName contents' = do
    let re = fromRight' (\_ -> unsafeCrashWith "Unexpected Left") $ DSR.regex "data:.*;base64," DSRF.noFlags
        contents = DSR.replace re "" contents'
    eTask :: Either RESTError GT.AsyncTask <- postWwwUrlencoded session p (bodyParams contents)
    pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.UploadFile }) <$> eTask
  where
    p = GR.NodeAPI GT.Node (Just id) $ GT.asyncTaskTypePath GT.UploadFile

    bodyParams c = [ Tuple "_wfi_b64_data"  (Just c)
                   , Tuple "_wfi_fileformat" (Just $ show fileFormat)
                   , Tuple "_wfi_name"      mName
                   ]

------------------------------------------------------------------------

uploadTermListView :: R2.Component Props
uploadTermListView = R.createElement uploadTermListViewCpt
uploadTermListViewCpt :: R.Component Props
uploadTermListViewCpt = here.component "uploadTermListView" cpt
  where
    cpt { dispatch, nodeType } _ = do
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
  , mFile    :: T.Box (Maybe UploadFile)
  , nodeType :: GT.NodeType
  , uploadType :: T.Box FileType
  )

uploadTermButton :: R2.Leaf UploadTermButtonProps
uploadTermButton = R2.leafComponent uploadTermButtonCpt
uploadTermButtonCpt :: R.Component UploadTermButtonProps
uploadTermButtonCpt = here.component "uploadTermButton" cpt
  where
    cpt { dispatch
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
        onClick mFile' uploadType' _ = do
          let {name, blob} = unsafePartial $ fromJust mFile'
          void $ launchAff do
            contents <- readUFBAsText blob
            _ <- dispatch $ UploadFile nodeType uploadType' Plain EN (Just name) contents (SelectedLists [])
            liftEffect $ do
              T.write_ Nothing mFile
------------------------------------------------------------------------

uploadFrameCalcView :: R2.Component Props
uploadFrameCalcView = R.createElement uploadFrameCalcViewCpt
uploadFrameCalcViewCpt :: R.Component Props
uploadFrameCalcViewCpt = here.component "uploadFrameCalcView" cpt
  where
    cpt { dispatch, session } _ = do
      lang' /\ langBox
        <- R2.useBox' EN
      selection' /\ selectionBox
        <- R2.useBox' ListSelection.MyListsFirst

      let bodies = [ 
        H.div 
        { className: "col-12 flex-space-around" }
        [ H.h4 {}
          [ H.text "This will upload current calc as Corpus CSV" ]
        ]
      ,
        -- Lang
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
            "File lang"
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formSelect'
            { callback: flip T.write_ langBox
            , value: lang'
            , list: [ EN, FR, No_extraction, Universal ]
            }
            []
          ]
        ]
      ,  
        -- List selection
        H.div
        { className: "form-group" }
        [
          H.div
          { className: "form-group__label" }
          [
            B.label_ $
              "List selection"
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            ListSelection.selection
            { selection: selectionBox
            , session
            } []
          ]
        ]
      ]

      let footer = H.div {}
                   [ H.button { className: "btn btn-primary"
                              , on: { click: onClick lang' selection' } }
                     [ H.text "Upload!" ]
                   ]

      pure $ panel bodies footer

      where
        onClick lang' selection' _ = do
          void $ launchAff do
            dispatch $ UploadFrameCalc lang' selection'

uploadFrameCalc :: Session
                    -> ID
                    -> Lang
                    -> ListSelection.Selection
                    -> AffRESTError GT.AsyncTaskWithType
uploadFrameCalc session id lang selection = do
  let p = GR.NodeAPI GT.Node (Just id) $ GT.asyncTaskTypePath GT.UploadFrameCalc
  let body = [
    Tuple "_wf_lang"       (Just $ show lang)
  , Tuple "_wf_selection"  (Just $ show selection)
  ]

  eTask <- postWwwUrlencoded session p body
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.UploadFrameCalc }) <$> eTask

