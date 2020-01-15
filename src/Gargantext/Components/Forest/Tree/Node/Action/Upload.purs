module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Prelude (class Show, Unit, const, discard, map, pure, show, ($), (<>), bind, void)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import URI.Query as Q
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, postWwwUrlencoded, postMultipartFormData)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..), AsyncTask(..))
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2

type Props =
  ( id :: Int
  , session :: Session
  )


uploadFileView :: (Action -> Aff Unit) -> Record Props -> R.Element
uploadFileView d props = R.createElement (uploadFileViewCpt d) props []

uploadFileViewCpt :: (Action -> Aff Unit) -> R.Component Props
uploadFileViewCpt d = R.hooksComponent "UploadFileView" cpt
  where
    cpt {id} _ = do
      mContents :: R.State (Maybe UploadFileContents) <- R.useState' Nothing
      fileType :: R.State FileType <- R.useState' CSV

      pure $ H.div {} [
        H.div {} [ H.text "Upload file!" ]
      , H.div {} [ H.input {type: "file", placeholder: "Choose file", on: {change: onChangeContents mContents}} ]
      , H.div {}
        [ R2.select {className: "col-md-12 form-control"
                    , on: {change: onChangeFileType fileType}
                    }
          (map renderOption [CSV, PresseRIS])
        ]
      , H.div {}
        [ uploadButton d id mContents fileType ]
      ]

    renderOption opt = H.option {} [ H.text $ show opt ]

    onChangeContents (mContents /\ setMContents) e = do
      blob <- R2.inputFileBlob e
      E.preventDefault e
      E.stopPropagation e
      void $ launchAff do
        contents <- readAsText blob
        liftEffect $ do
          setMContents $ const $ Just $ UploadFileContents contents

    onChangeFileType (fileType /\ setFileType) e = do
      setFileType $ const $ unsafePartial $ fromJust $ readFileType $ R2.unsafeEventValue e

uploadButton :: (Action -> Aff Unit) -> Int -> R.State (Maybe UploadFileContents) -> R.State FileType -> R.Element
uploadButton d id (mContents /\ setMContents) (fileType /\ setFileType) =
  H.button {className: "btn btn-primary", disabled, on: {click: onClick}} [ H.text "Upload" ]
  where
    disabled = case mContents of
      Nothing -> "1"
      Just _ -> ""

    onClick e = do
      let contents = unsafePartial $ fromJust mContents
      void $ launchAff do
        _ <- d $ UploadFile fileType contents
        liftEffect $ do
          setMContents $ const $ Nothing
          setFileType $ const $ CSV

-- START File Type View
type FileTypeProps =
  ( id :: ID
  , nodeType :: NodeType)

fileTypeView :: (Action -> Aff Unit)
             -> Record FileTypeProps
             -> R.State (Maybe DroppedFile)
             -> R.State Boolean
             -> R.Element
fileTypeView d p (Just (DroppedFile {contents, fileType}) /\ setDroppedFile) (_ /\ setIsDragOver) = R.createElement el p []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt {id} _ = do
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
            (map renderOption [CSV, PresseRIS])
          ]
          where
            onChange e =
              setDroppedFile $ const $ Just $ DroppedFile $ {contents, fileType: readFileType $ R2.unsafeEventValue e}
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
                                   launchAff $ d $ UploadFile ft contents
                               }
                         } [H.text "Upload"]
              Nothing ->
                H.button {className: "btn btn-success disabled"
                         , type: "button"
                         } [H.text "Upload"]
          ]
fileTypeView _ _ (Nothing /\ _) _ = R.createElement el {} []
  where
    el = R.hooksComponent "FileTypeView" cpt
    cpt props _ = pure $ H.div {} []

newtype FileUploadQuery = FileUploadQuery {
    fileType :: FileType
  }
derive instance newtypeSearchQuery :: Newtype FileUploadQuery _
instance fileUploadQueryToQuery :: ToQuery FileUploadQuery where
  toQuery (FileUploadQuery {fileType}) =
    QP.print id id $ QP.QueryPairs $
         pair "fileType" fileType
    where pair :: forall a. Show a => String -> a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k v = [ QP.keyFromString k /\ (Just $ QP.valueFromString $ show v) ]

uploadFile :: Session -> ID -> FileType -> UploadFileContents -> Aff AsyncTask
uploadFile session id fileType (UploadFileContents fileContents) =
    --postWwwUrlencoded session p fileContents
    postMultipartFormData session p fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    p = NodeAPI Corpus (Just id) $ "add/file/async" <> Q.print (toQuery q)
