module Gargantext.Components.Nodes.Home.Public where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String (take)
import Gargantext.Config (publicBackend)
import Gargantext.Config.REST (AffRESTError, get, logRESTError)
import Gargantext.Ends (backendUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.SimpleJSON as GUSJ
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Home.Public"

type PublicDataProps = ( publicData :: Array PublicData )

data PublicData = PublicData
  { title    :: String
  , abstract  :: String
  , img      :: String
  , url      :: String
  , date     :: String
  , database :: String
  , author   :: String
  } | NoData { nodata :: String }

derive instance Eq PublicData
derive instance Generic PublicData _
instance JSON.ReadForeign PublicData where readImpl = GUSJ.taggedSumRep
instance Show PublicData where show = genericShow

------------------------------------------------------------------------
type LoadData :: forall k. Row k
type LoadData  = ()

type LoadProps = (reload :: Int)

-- | WIP still finding the right way to chose the default public backend
loadPublicData :: Record LoadProps -> AffRESTError (Array PublicData)
loadPublicData _l = do
  -- This solution is error prone (url needs to be cleaned)
  --backend <- liftEffect public

  -- This solution for development only, with local backend
  -- let backend = head defaultBackends
  let backend = publicBackend
  get Nothing (backendUrl backend "public")

{- | Another solution: get all data
  let
    ok = ["local.cnrs", "devel.inshs.cnrs"]
    backends = Array.filter
      (\(Backend {name}) -> Array.elem name ok)
     (toArray defaultBackends)

  Array.concat <$> traverse
    (\backend -> get Nothing (backendUrl backend "public"))
    backends
-}

renderPublic :: R2.Leaf ()
renderPublic = R2.leafComponent renderPublicCpt
renderPublicCpt :: R.Component ()
renderPublicCpt = here.component "renderPublic" cpt where
  cpt _ _ = do
    useLoader { errorHandler
              , loader: loadPublicData
              , path: { reload: 0 }
              , render:  loaded }
      where
        loaded publicData = publicLayout { publicData }
        errorHandler = logRESTError here "[renderPublic]"

publicLayout :: Record PublicDataProps -> R.Element
publicLayout props = R.createElement publicLayoutCpt props []
publicLayoutCpt :: R.Component PublicDataProps
publicLayoutCpt = here.component "publicLayout" cpt
  where
    cpt { publicData } _ = do
      pure $
        H.span {}
        [ H.div { className: "text-center" }
          [ H.div { style: { marginBottom: "15px" }}
            [ H.h2 {}
              [ H.text "Discover knowledge"
              , H.p { className: "lead text-muted center"}
                [ H.text "maps made with "
                , H.span { className: "fa fa-heart" } [] ]]]]
        -- | TODO: browse maps, random maps
        , album publicData ]

album :: Array PublicData -> R.Element
album pd =
  H.div { className: "album py-5 bg-light" }
  [ H.div { className: "container" }
    [ H.div { className: "row" } (map content pd) ]]
  where content tab = H.div { className: "col-md-6 content"} [ tableau tab ]

tableau :: PublicData -> R.Element
tableau (NoData _) =
  H.div { className : "center" }
  [ H.h2 {} [ H.text "Create a corpus and publicize it" ]]
tableau (PublicData { title, abstract, img, url, date, database, author }) =
  H.div { className: "card mb-6 box-shadow" }
  [ H.a { target: "_blank", href: url }
    [ H.div { className:"center"} [ H.img { src: img, width: "50%" } ]]
  , H.div { className : "card-body"}
    [ H.h3 {} [H.text title]
    , H.p { className: "card-text"} [ H.text $ (take 252 abstract) <> "..." ]
    , H.div { className: "center justify-content-between align-items-center" }
      [ H.div { className: "btn-group" }
        [ H.div { className : "small text-muted d-flex justify-content-flex-end" }
          [ H.text
            $  "Made by " <> author
            <> " on "     <> date
            <> " with "   <> database ]]]]]
        -- , H.button { className: "btn btn-primary flex-between"
        --            , href: url, role: "button" }
        --            [ H.text "View the map" ]
        -- , H.button { className: "btn btn-primary flex-start"
        --            , href: url, role: "button" }
        --            [ H.text "More like this" ]]]]
