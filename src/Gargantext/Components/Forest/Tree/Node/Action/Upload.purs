module Gargantext.Components.Forest.Tree.Node.Action.Upload where

import Data.Newtype (class Newtype)
import Effect.Aff (Aff, launchAff)
import Gargantext.Sessions (Session, postWwwUrlencoded)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..))
import Gargantext.Routes (SessionRoute(..))
import Prelude (class Show, Unit, const, discard, map, pure, show, ($), (<>))
import Data.Maybe (Maybe(..))
import URI.Extra.QueryPairs as QP
import Gargantext.Components.Forest.Tree.Node.Action
import Reactix as R
import Data.Tuple (Tuple)
import URI.Query as Q
import Reactix.DOM.HTML as H
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2
import Data.Tuple.Nested ((/\))

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
                    , onClick: mkEffectFn1 $ \_ -> do
                        setDroppedFile $ const Nothing
                        setIsDragOver $ const false
                    , title: "Close"} []
              ]
            ]
          ]
        panelBody =
          H.div {className: "panel-body"}
          [ R2.select {className: "col-md-12 form-control"
                      , onChange: onChange}
            (map renderOption [CSV, PresseRIS])
          ]
          where
            onChange = mkEffectFn1 $ \e ->
              setDroppedFile $ const $ Just $ DroppedFile $ {contents, fileType: readFileType $ e .. "target" .. "value"}
        renderOption opt = H.option {} [ H.text $ show opt ]
        panelFooter =
          H.div {className: "panel-footer"}
          [
            case fileType of
              Just ft ->
                H.button {className: "btn btn-success"
                         , type: "button"
                         , onClick: mkEffectFn1 $ \_ -> do
                             setDroppedFile $ const Nothing
                             launchAff $ d $ UploadFile ft contents
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

uploadFile :: Session -> ID -> FileType -> UploadFileContents -> Aff (Array FileHash)
uploadFile session id fileType (UploadFileContents fileContents) =
    postWwwUrlencoded session p fileContents
  where
    q = FileUploadQuery { fileType: fileType }
    p = NodeAPI Node (Just id) $ "upload" <> Q.print (toQuery q)

