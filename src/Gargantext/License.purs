module Gargantext.License where

import Reactix as R
import Reactix.DOM.HTML as H

license :: R.Element
license = H.p { className: "license" }
            [ H.text "GarganText "
            , H.span { className: "fa fa-registered"} []
            , H.text " is made by "
            , H.a { href: "https://iscpif.fr"
                  , target: "blank"
                  } [ H.text "CNRS/ISCPIF" ]
            , H.a { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                  , target: "blank"
                  , title: "Legal instructions of the project."
                  }
              [ H.text ", with licences aGPLV3 and CECILL variant Affero compliant, " ]
            , H.span { className: "fa fa-copyright"   } []
            , H.a { href: "https://cnrs.fr", target:"blank"} [H.text " CNRS 2017-Present "]
            , H.text "."
            ]

