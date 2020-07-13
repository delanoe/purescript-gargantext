module Gargantext.Components.Nodes.Home.Public where

import DOM.Simple.Console (log)
import Data.Tuple (fst)
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (head)
import Data.String (take)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Config (publicBackend)
import Gargantext.Config.REST (get)
import Gargantext.Ends (backendUrl)
import Gargantext.Prelude
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Reactix as R
import Reactix.DOM.HTML as H


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

loadPublicData :: Record LoadProps -> Aff (Array PublicData)
loadPublicData _l = do
  backend <- liftEffect publicBackend
  _ <- liftEffect (log backend)
  get Nothing (backendUrl backend "public")

renderPublic :: R.Element
renderPublic = R.createElement renderPublicCpt {} []

renderPublicCpt :: R.Component LoadData
renderPublicCpt = R.hooksComponent "G.C.N.Home.Public.renderPublic" cpt
  where
    cpt {} _ = do
      reload <- R.useState' 0
{-
      (pds /\ setPds) :: R.State PublicProps <- R.useState' ( replicate 6 (PublicData { title: "Title"
                                                    , abstract : foldl (<>) "" $ replicate 100 "abstract "
                                                    , img: "images/Gargantextuel-212x300.jpg"
                                                    , url : "https://.."
                                                    , date: "YY/MM/DD"
                                                    , database: "database"
                                                    , author  : "Author"
                                                    }
                                        )
                          )
                          -}
      useLoader { reload: fst reload } loadPublicData (\pd -> publicLayout {publicDatas: pd})


------------------------------------------------------------------------
publicLayout :: Record PublicProps -> R.Element
publicLayout props = R.createElement publicLayoutCpt props []

publicLayoutCpt :: R.Component PublicProps
publicLayoutCpt = R.hooksComponent "[G.C.N.H.Public.publicLayout" cpt
  where
    cpt {publicDatas} _ = do
      pure $ H.span {}
             [ H.div { className: "text-center" }
                     [ H.div { className:"container1" }
                             [ H.h2 {} [H.text "Public Maps"]
                             , H.p { className: "lead text-muted"}
                                   [ H.text "Discover maps made with "
                                   , H.div {className: "fa fa-heart"} []
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
