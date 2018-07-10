module Gargantext.Components.Data.Landing where


data LandingData = LandingData { name       :: String
                         , signature  :: String
                         , logoTitle  :: String
                         , imageTitle :: String
                         , blockTexts :: BlockTexts
                         }

data BlockTexts = BlockTexts { blocks :: Array BlockText }

data BlockText = BlockText { title      :: String
                           , href       :: String
                           , titleText  :: String
                           , icon       :: String
                           , text       :: String
                           , docButton  :: Button
                          }


data Button = Button { title :: String
                     , text  :: String
                     , href  :: String
                   }
