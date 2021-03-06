{-# LANGUAGE DeriveGeneric
  , OverloadedStrings
  , TypeApplications
  , DeriveAnyClass
#-}
module GRACeGraph where
import System.Environment
import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import GHC.Generics
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as FingBS
import qualified Data.Text as T

-- | An adjacency list representation of graphs
data Graph = Graph { nodes :: [Node] }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

-- | Nodes in the graph
data Node = Node { identity   :: Maybe Int   -- * What ID does this node have?
                 , name       :: String      -- * What is the name of the GL component?
                 , parameters :: [Parameter] -- * What are the parameters of this node?
                 , interface  :: [Interface] -- * What is the interface of this node?
                 }
  deriving (Generic, Show, Eq)

nodeOptions :: Options
nodeOptions = defaultOptions {omitNothingFields = True}

-- | Obvious instances
instance ToJSON Node where
  toJSON = genericToJSON nodeOptions

-- | Obvious instances
instance FromJSON Node where
  parseJSON  = genericParseJSON  nodeOptions

data Parameter = Parameter { parameterName  :: String
                           , parameterType  :: PrimType
                           , parameterValue :: Maybe PrimTypeValue
                           }
  deriving (Generic, Show, Eq)

parameterOptions :: Options
parameterOptions = defaultOptions { fieldLabelModifier = (map toLower) . (drop $ length ("parameter" :: String)), omitNothingFields = True }

instance ToJSON Parameter where
  toJSON = genericToJSON parameterOptions

instance FromJSON Parameter where
  parseJSON  = genericParseJSON  parameterOptions

-- | The primitive types in `GRACe`
data PrimType = FloatT | IntT | StringT | BoolT
  deriving (Generic, Show, Eq)

primTypeOptions :: Options
primTypeOptions = defaultOptions { constructorTagModifier = init }

instance ToJSON PrimType where
  toJSON = genericToJSON primTypeOptions

instance FromJSON PrimType where
  parseJSON  = genericParseJSON  primTypeOptions

data PrimTypeValue = FloatV Float | IntV Int | StringV String | BoolV Bool
  deriving (Generic, Show, Eq)

-- | PrimTypeValue are represented in JSON as just String/Bool/Number
instance ToJSON PrimTypeValue where
  toJSON (FloatV f)  = Number $ fromRational (toRational f)
  toJSON (IntV i)    = Number $ fromInteger (toInteger i)
  toJSON (StringV s) = String (T.pack s)
  toJSON (BoolV b)   = Bool b

-- | Derived from the above instance
instance FromJSON PrimTypeValue where
  parseJSON (String s) = return $ StringV (T.unpack s)
  parseJSON (Bool b)   = return $ BoolV b
  parseJSON (Number s) = return $ either FloatV IntV $ floatingOrInteger s
  parseJSON _          = fail "Does not comform to interface"

-- | Fix the prefixes
data Interface = Interface { interfaceName       :: String
                           , interfaceType       :: String
                           , interfaceConnection :: Maybe (Int, String)
                           }
  deriving (Generic, Show, Eq)

interfaceOptions :: Options
interfaceOptions = defaultOptions { fieldLabelModifier = (map toLower) . (drop (length ("interface" :: String))), omitNothingFields = True }

instance ToJSON Interface where
  toJSON = genericToJSON interfaceOptions

instance FromJSON Interface where
  parseJSON  = genericParseJSON  interfaceOptions

example = Graph
  [ Node
     (Just 1)
     "pump"
     [ Parameter "capacity" FloatT (Just (FloatV 5))
     ]
     [ Interface "inflow"  "flow"  (Just (3, "outlet"))
     , Interface "outflow" "flow"  Nothing
     ]
  , Node
     (Just 2)
     "rain"
     [ Parameter "amount"   FloatT (Just (FloatV 10))
     ]
     [ Interface "rainfall" "flow" (Just (3, "inflow"))
     ]
  , Node
     (Just 3)
     "runoffArea"
     [ Parameter "capacity" FloatT (Just (FloatV 5))
     ]
     [ Interface "inflow"   "flow" Nothing
     , Interface "outlet"   "flow" Nothing
     , Interface "overflow" "flow" Nothing
     ]
  ]

pPrintPTV :: PrimTypeValue -> String
pPrintPTV (FloatV f)  = show f
pPrintPTV (IntV i)    = show i
pPrintPTV (StringV s) = show s
pPrintPTV (BoolV b)   = show b
