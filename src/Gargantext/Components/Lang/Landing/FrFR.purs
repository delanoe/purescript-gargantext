module Gargantext.Components.Lang.Landing.FrFR where

import Gargantext.Components.Data.Landing

landingData :: LandingData
landingData =
  LandingData
    { name: "Gargantext"
    , signature: "chercher cartographier partgager"
    , logoTitle: "Projet développé par le CNRS (France, Europe)"
    , imageTitle: "Cliquez et testez vous-mêmes"
    , blockTexts: BlockTexts { blocks: blockTexts }
    }

blockTexts :: Array BlockText
blockTexts =
  [ BlockText
      { title: "Phrases aléatoires issues de l'oeuvre de François Rabelais. L'ordre historique des chapitres est préservé."
      , href: "#"
      , icon: "glyphicon glyphicon-random"
      , titleText: "Historique"
      , text: "Chapitre 1"
      , docButton:
        Button
          { title: "Your first map in less than 5 minutes"
          , text: " Documentation"
          , href: "https://iscpif.fr/gargantext/your-first-map/"
          }
      }
  , BlockText
      { title: "Mots aléatoires."
      , href: "#"
      , icon: "glyphicon glyphicon-random"
      , titleText: "Presentation"
      , text: "Autem nascetur iaculis, sedfusce enimsed cursus posuere consectetuer eu justo aliquammauris. Phasellus vero nisi porttitor elit quod, leo feliscras ultricies non tempor sagittis. Liberoduis facilisinam erat dapibusnam, lacus dui duis tristique volutpatut quis vestibulum magna. Nobis faucibusvestibulum dolores minim. Bibendumin malesuada adipiscing ante, mattis fames nequeetiam lorem. No diam id. Litora quisaenean commodo lobortisetiam neque, libero mollis scelerisque inceptos ullamcorper sea congue delenit possim."
      , docButton:
        Button
          { title: "Your first map in less than 5 minutes"
          , text: " Documentation"
          , href: "https://iscpif.fr/gargantext/your-first-map/"
          }
      }
  , BlockText
      { title: "Lettres alétaoires, expérience"
      , href: "#"
      , icon: "glyphicon glyphicon-random"
      , titleText: "Tutoreil"
      , text: "Il paraît que l'rdore des lettres dans un mot n'a pas d'imtraopnce. La première et la dernière lettre doeivnt être à la bonne place. Le reste peut être dans un désordre total et on peut touojurs lire sans prolèbme. On ne lit donc pas chaque lettre en ellêem-me, mais le mot comme un tout. Un chaegmnent de référentiel et nous tranpossons ce résultat au texte luimê-me: l'rdore des mots est failbement important copamré au contexte du texte qui, lui, est copmté: comptexter avec Gargantext."
      , docButton:
        Button
          { title: "Your first map in less than 5 minutes"
          , text: " Documentation"
          , href: "https://iscpif.fr/gargantext/your-first-map/"
          }
      }
  ]
