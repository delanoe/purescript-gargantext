module Gargantext.Components.Nodes.Home.Public where

import Data.String (take)
import Gargantext.Prelude
import Data.Tuple.Nested ((/\))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Sessions (Session(..))


type PublicProps = (publicDatas :: R.State (Array PublicData)
                   -- , session :: Session
                   )

newtype PublicData = PublicData
  { title    :: String
  , abstract  :: String
  , img      :: String
  , url      :: String
  , date     :: String
  , database :: String
  , author   :: String
  }


publicLayout :: Record PublicProps -> R.Element
publicLayout props = R.createElement publicLayoutCpt props []

publicLayoutCpt :: R.Component PublicProps
publicLayoutCpt = R.staticComponent "[G.C.N.H.Public.publicLayout" cpt
  where
    cpt {publicDatas} _ =
      H.span {}
             [ H.div { className: "container1" } [ H.h2 {} [H.text "Public Maps"]]
             -- | TODO browse maps
             -- | TODO random maps
             , album pds
             ]
        where
          (pds /\ _setPublicData) = publicDatas

album :: Array PublicData -> R.Element
album pd = H.div {className: "album py-5 bg-light"}
                 [ H.div { className: "container" }
                         [ H.div { className : "row" }
                                 (map (\tab -> H.div {className : "col-md-6 content"} [tableau tab]) pd )
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
