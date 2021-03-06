module Gargantext.License where

import Reactix as R
import Reactix.DOM.HTML as H

license :: R.Element
license = H.p {}
            [ H.text "Gargantext "
            , H.span { className: "glyphicon glyphicon-registration-mark"} []
            , H.text " is made by "
            , H.a { href: "https://iscpif.fr"
                  , target: "blank"
                  } [ H.text "CNRS/ISCPIF" ]
            , H.a { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                  , target: "blank"
                  , title: "Legal instructions of the project."
                  }
              [ H.text ", with licences aGPLV3 and CECILL variant Affero compliant, " ]
            , H.span { className: "glyphicon glyphicon-copyright-mark"   } []
            , H.a { href: "https://cnrs.fr", target:"blank"} [H.text " CNRS 2017-Present "]
            , H.text "."
            ]

