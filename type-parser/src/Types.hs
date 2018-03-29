{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Identifier
  , TypeDef(..)
  , TypeParts(..)
  , Type(..)
  , PrimitiveType(..)
  , FunDef(..)
  , DefFile(..)
  , decodeType
  , generateDefFile
  ) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (pack, unpack)

import GHC.Generics

type Identifier = [String]

data NamedTypeDef = NamedTypeDef
  { typeName :: Identifier
  , parts :: NamedType
  } deriving (Eq, Show)

data Type
  = Named Identifier
  | Unnamed UnnamedType

data NamedType
  = SumParts [(Identifier, Type)]
  | ProdParts [(Identifier, Type)]
  | Opaque
  deriving (Eq, Generic, Show)

data UnnamedType
  = Any Identifier
  | PrimitiveType

type Variable = (Identifier, Type)

data PrimitiveType
  = Unit
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | UInt8
  | UInt16
  | UInt32
  | UInt64
  | Float16
  | Float32
  | Float64
  | CodepointUnicode
  | StringUTF8
  | Function [Variable] Type
  | List Type
  deriving (Eq, Generic, Show)

data FunDef = FunDef
  { funName :: Identifier
  , sources :: [Type]
  , target :: Type
  } deriving (Eq, Generic, Show)

data DefFile = DefFile
  { libName :: Identifier
  , funDefs :: [FunDef]
  , typeDefs :: [TypeDef]
  } deriving (Eq, Generic, Show)

decodeType :: String -> Maybe DefFile
decodeType = decode . pack

generateDefFile :: DefFile -> String
generateDefFile = unpack . encodePretty

instance ToJSON TypeDef where
  toJSON x = object ["name" .= typeName x, partsType .= parts x]
    where
      partsType =
        case parts x of
          SumParts _ -> "sumParts"
          ProdParts _ -> "prodParts"
          Unit -> "prodParts"

instance ToJSON TypeParts where
  toJSON (SumParts x) = toJSON x
  toJSON (ProdParts x) = toJSON x
  toJSON Unit = toJSON ([] :: [String])

instance FromJSON TypeDef where
  parseJSON = withObject "TypeDef" $ \v -> toTypeDef <$> v .: "name" <*> v .:? "sumParts" <*> v .:? "prodParts"

toTypeDef :: Identifier -> Maybe [(Identifier, Type)] -> Maybe [(Identifier, Type)] -> TypeDef
toTypeDef name (Just x) _ = TypeDef name $ SumParts x
toTypeDef name _ (Just x) = TypeDef name $ ProdParts x
toTypeDef name _ _ = TypeDef name Unit

instance FromJSON FunDef

instance ToJSON FunDef

instance FromJSON DefFile

instance ToJSON DefFile

instance FromJSON Type

instance ToJSON Type

instance FromJSON PrimitiveType

instance ToJSON PrimitiveType
