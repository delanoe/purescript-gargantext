module Gargantext.Components.Nodes.Home.Public where

import Data.String (take)
import Gargantext.Prelude
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H


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
             [ H.div { className: "text-center" }
                     [ H.div { className:"container1" }
                             [ H.h2 {} [H.text "Public Maps"]
                             , H.p {className: "lead text-muted"} [H.text "Discover maps made with "
                                   , H.div {className: "fa fa-heart"} []]
                             , H.p { className:"flex-space-around" }
                                   [ H.a { className: "btn btn-primary my-2"
                                         , href :"https://gargantext.org"
                                         } [H.text "Join"]
                                   ]
                             ]
                     ]
             -- | TODO browse maps
             -- | TODO random maps
             , album pds
             ]
        where
          (pds /\ _setPublicData) = publicDatas

{-
   <section class="jumbotron text-center">
      <div class="container" >
        <h1 class="jumbotron-heading">Gargantext Maps</h1>
        <p class="lead text-muted">Discover maps made with love</p>
        <p>
          <a href="http://gargantext.org" target="blank" class="btn btn-primary my-2">More about Gargantext</a>
          <a href="http://iscpif.fr" target="blank"  class="btn btn-secondary my-2">CNRS/ISC-PIF</a>
        </p>
      </div>
    </section>
    -}
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
