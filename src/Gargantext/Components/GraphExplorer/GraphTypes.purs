module Gargantext.Components.GraphExplorer.GraphTypes where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Record as Record
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))


newtype Cluster = Cluster { clustDefault :: Int }

derive instance Generic Cluster _
derive instance Newtype Cluster _
instance Eq Cluster where eq = genericEq
instance JSON.ReadForeign Cluster where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Cluster $ Record.rename clust_defaultP clustDefaultP inst
instance JSON.WriteForeign Cluster where
  writeImpl (Cluster cl) = JSON.writeImpl $ Record.rename clustDefaultP clust_defaultP cl

newtype ClusterCount = ClusterCount
  { id    :: Int
  , count :: Int
  }
derive instance Generic ClusterCount _
derive instance Newtype ClusterCount _


newtype Node = Node {
    attributes :: Cluster
  , children   :: Array String
  , id_        :: String
  , label      :: String
  , size       :: Int
  , type_      :: String
  , x          :: Number
  , y          :: Number
  }

x_coordP = Proxy :: Proxy "x_coord"
xP = Proxy :: Proxy "x"
y_coordP = Proxy :: Proxy "y_coord"
yP = Proxy :: Proxy "y"
clustDefaultP = Proxy :: Proxy "clustDefault"
clust_defaultP = Proxy :: Proxy "clust_default"
cameraP = Proxy :: Proxy "camera"
mCameraP = Proxy :: Proxy "mCamera"
idP = Proxy :: Proxy "id"
id_P = Proxy :: Proxy "id_"
typeP = Proxy :: Proxy "type"
type_P = Proxy :: Proxy "type_"

derive instance Generic Node _
derive instance Newtype Node _
instance Eq Node where eq = genericEq
instance Ord Node where compare (Node n1) (Node n2) = compare n1.id_ n2.id_
instance JSON.ReadForeign Node where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Node $
      Record.rename idP id_P $
      Record.rename typeP type_P $
      Record.rename x_coordP xP $
      Record.rename y_coordP yP $ inst
instance JSON.WriteForeign Node where
  writeImpl (Node nd) = JSON.writeImpl $
                        Record.rename id_P idP $
                        Record.rename type_P typeP $
                        Record.rename xP x_coordP $
                        Record.rename yP y_coordP nd


newtype Edge = Edge {
    confluence :: Number
  , id_ :: String
  , source :: String
  , target :: String
  , weight :: Number
  }


derive instance Generic Edge _
derive instance Newtype Edge _
instance Eq Edge where eq = genericEq
instance Ord Edge where compare (Edge e1) (Edge e2) = compare e1.id_ e2.id_
instance JSON.ReadForeign Edge where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Edge $ Record.rename idP id_P inst
instance JSON.WriteForeign Edge where
  writeImpl (Edge ed) = JSON.writeImpl $ Record.rename id_P idP ed
