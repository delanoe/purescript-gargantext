module Gargantext.License where

import Prelude
import Reactix as R
import Reactix.DOM.HTML as H

license :: R.Element
license = H.p {}
            [ H.text "Gargantext "
            , H.span {className: "glyphicon glyphicon-registration-mark"} []
            , H.a { href: "http://www.cnrs.fr"
                  , target: "blank"
                  , title: "Project hosted by CNRS."
                  }
              [ H.text ", Copyrights "
              , H.span { className: "glyphicon glyphicon-copyright-mark" } []
              , H.text " CNRS 2017-Present"
              ]
            , H.a { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                  , target: "blank"
                  , title: "Legal instructions of the project."
                  }
              [ H.text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
            , H.text "."
            ]

