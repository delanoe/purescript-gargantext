module Gargantext.Components.Lang.Landing.EnUS where

import Gargantext.Components.Data.Landing

landingData :: LandingData
landingData = LandingData { name      : "Gargantext"
                    , signature : "search map share"
                    , logoTitle : "Project hosted by CNRS (France, Europa)"
                    , imageTitle: "Click and test by yourself"
                    , blockTexts : BlockTexts { blocks : blockTexts}
                                         }


blockTexts :: Array BlockText
blockTexts = [ BlockText { title : "Random sentences in Gargantua's Books chapters, historically true"
                         , href  : "#"
                         , icon  : "fa fa-random"
                         , titleText : "Historic"
                         , text  : "Chapter 1.XV. How Gargantua was put under other schoolmasters. Chapter 2.XXII. How Panurge served a Parisian lady a trick that pleased her not very well. Chapter 3.XXXVII. How Pantagruel persuaded Panurge to take counsel of a fool. Chapter 4.LXI. How Gaster invented means to get and preserve corn. Chapter 5.XXXVIII. Of the temple's admirable pavement."
                         , docButton : Button { title : "Your first map in less than 5 minutes"
                                              , text  : " Documentation"
                                              , href  : "https://iscpif.fr/gargantext/your-first-map/"
                                              }
                         }
             , BlockText { title : "Randomized words, semantically and syntaxically falses."
                         , href  : "#"
                         , icon  : "fa fa-random"
                         , titleText : "Presentation"
                         , text  : "Autem nascetur iaculis, sedfusce enimsed cursus posuere consectetuer eu justo aliquammauris. Phasellus vero nisi porttitor elit quod, leo feliscras ultricies non tempor sagittis. Liberoduis facilisinam erat dapibusnam, lacus dui duis tristique volutpatut quis vestibulum magna. Nobis faucibusvestibulum dolores minim. Bibendumin malesuada adipiscing ante, mattis fames nequeetiam lorem. No diam id. Litora quisaenean commodo lobortisetiam neque, libero mollis scelerisque inceptos ullamcorper sea congue delenit possim."
                         , docButton : Button { title : "Your first map in less than 5 minutes"
                                               , text  : " Documentation"
                                               , href  : "https://iscpif.fr/gargantext/your-first-map/"
                                               }
                         }
             , BlockText { title : "Randomized letters, true or false ?"
                         , href  : "#"
                         , icon  : "fa fa-random"
                         , titleText : "Tutoreil"
                         , text  : "Il paraît que l'rdore des lettres dans un mot n'a pas d'imtraopnce. La première et la dernière lettre doeivnt être à la bonne place. Le reste peut être dans un désordre total et on peut touojurs lire sans prolèbme. On ne lit donc pas chaque lettre en ellêem-me, mais le mot comme un tout. Un chaegmnent de référentiel et nous tranpossons ce résultat au texte luimê-me: l'rdore des mots est failbement important copamré au contexte du texte qui, lui, est copmté: comptexter avec Gargantext."
                         , docButton : Button { title : "Your first map in less than 5 minutes"
                                               , text  : " Documentation"
                                               , href  : "https://iscpif.fr/gargantext/your-first-map/"
                                               }
                                    }
              ]
