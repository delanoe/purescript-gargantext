module Gargantext.Components.Nodes.Home.Public where

import Data.Array.NonEmpty (toArray)
import Data.Array as Array
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (head)
import Data.String (take)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Config (publicBackend)
import Gargantext.Config.REST (get)
import Gargantext.Ends (backendUrl, Backend(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Home.Public"

type PublicProps = (publicDatas :: (Array PublicData)
                   -- , session :: Session
                   )

data PublicData = PublicData
  { title    :: String
  , abstract  :: String
  , img      :: String
  , url      :: String
  , date     :: String
  , database :: String
  , author   :: String
  } | NoData { nodata :: String }

derive instance eqPublicData :: Eq PublicData

derive instance genericPublicData :: Generic PublicData _

instance showPublicData :: Show PublicData where
  show = genericShow

instance decodeJsonPublicData :: Argonaut.DecodeJson PublicData where
  decodeJson = genericSumDecodeJson

instance encodeJsonPublicData :: Argonaut.EncodeJson PublicData where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
type LoadData  = ()
type LoadProps = (reload :: Int)

-- | WIP still finding the right way to chose the default public backend
loadPublicData :: Record LoadProps -> Aff (Array PublicData)
loadPublicData _l = do
  -- This solution is error prone (url needs to be cleaned)
  --backend <- liftEffect publicBackend

  -- This solution for development only, with local backend
  -- let backend = head defaultBackends
  let backend = publicBackend
  get Nothing (backendUrl backend "public")

{- | Another solution: get all data
  let
    ok = ["local.cnrs", "devel.inshs.cnrs"]
    backends = Array.filter (\(Backend {name}) -> Array.elem name ok) (toArray defaultBackends)

  Array.concat <$> traverse (\backend -> get Nothing (backendUrl backend "public")) backends
-}

renderPublic :: R.Element
renderPublic = R.createElement renderPublicCpt {} []

renderPublicCpt :: R.Component LoadData
renderPublicCpt = R2.hooksComponent thisModule "renderPublic" cpt
  where
    cpt {} _ = do
      reload <- R.useState' 0
      useLoader { reload: fst reload } loadPublicData (\pd -> publicLayout {publicDatas: pd})


------------------------------------------------------------------------
publicLayout :: Record PublicProps -> R.Element
publicLayout props = R.createElement publicLayoutCpt props []

publicLayoutCpt :: R.Component PublicProps
publicLayoutCpt = R2.hooksComponent thisModule "publicLayout" cpt
  where
    cpt {publicDatas} _ = do
      pure $ H.span {}
             [ H.div { className: "text-center" }
                     [ H.div { className:"container1" }
                             [ H.h2 {} [H.text "Public Maps"]
                             , H.p { className: "lead text-muted"}
                                   [ H.text "Discover maps made with "
                                   , H.span {className: "fa fa-heart"} []
                                   ]
                             , H.p { className:"flex-space-around" }
                                   [ H.a { className: "btn btn-primary my-2"
                                         , href :"https://gargantext.org"
                                         } [H.text "Join"]
                                   ]
                             ]
                     ]
             -- | TODO browse maps
             -- | TODO random maps
             , album publicDatas
             ]

album :: Array PublicData -> R.Element
album pds = H.div {className: "album py-5 bg-light"}
                  [ H.div { className: "container" }
                          [ H.div { className : "row" }
                                  (map (\tab -> H.div {className : "col-md-6 content"}
                                                      [tableau tab]
                                       ) pds
                                  )
                          ]
                  ]


tableau :: PublicData -> R.Element
tableau (PublicData {title, abstract, img, url, date, database, author}) =
  H.div {className: "card mb-6 box-shadow"}
  [ H.a { target: "_blank", href: url } [ H.div { className:"center"}
                                                [H.img { src: img
                                                       , width: "50%"
                                                       }
                                                ]
                                        ]
        , H.div { className : "card-body"}
                [ H.h3 {} [H.text title]
                , H.p   { className: "card-text"} [H.text $ (take 252 abstract) <> "..."]
                , H.div { className: "center justify-content-between align-items-center"}
                        [ H.div { className: "btn-group" }
                                [ H.button { className : "btn btn-default flex-between"
                                           , href : url
                                           , role : "button"
                                           } [ H.text "View the map" ]
{- TODO
                                , H.button { className : "btn btn-default flex-start"
                                           , href : url
                                           , role : "button"
                                           } [ H.text "More like this" ]
-}
                                ]
                        , H.div { className : "small text-muted flex-end" } [ H.text $ "Made by " <> author
                                                                                    <> " on "     <> date
                                                                                    <> " with "   <> database
                                                                            ]
                        ]
                 ]
          ]
tableau (NoData {nodata}) = H.div {className : "center"} [H.h2 {} [H.text "Create a corpus and publicize it"]]
