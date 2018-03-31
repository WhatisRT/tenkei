{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types
  ( Identifier
  , NamedTypeDef(..)
  , NamedType(..)
  , UnnamedType(..)
  , Variable
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
  } deriving (Eq, Generic, Show)

data Type
  = Named Identifier
  | Unnamed UnnamedType
  deriving (Eq, Generic, Show)

data NamedType
  = SumParts [(Identifier, Type)]
  | ProdParts [(Identifier, Type)]
  | Opaque
  deriving (Eq, Generic, Show)

data UnnamedType
  = Any Identifier
  | Primitive PrimitiveType
  deriving (Eq, Generic, Show)

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
  , sources :: [Variable]
  , target :: Type
  } deriving (Eq, Generic, Show)

data DefFile = DefFile
  { libName :: Identifier
  , funDefs :: [FunDef]
  , typeDefs :: [NamedTypeDef]
  } deriving (Eq, Generic, Show)

decodeType :: String -> Maybe DefFile
decodeType = decode . pack

generateDefFile :: DefFile -> String
generateDefFile = unpack . encodePretty

instance ToJSON NamedTypeDef where
  toJSON x = object ["name" .= typeName x, partsType .= parts x]
    where
      partsType =
        case parts x of
          SumParts _ -> "sumParts"
          ProdParts _ -> "prodParts"
          Opaque -> "opaque"

instance ToJSON NamedType where
  toJSON (SumParts x) = toJSON x
  toJSON (ProdParts x) = toJSON x
  toJSON Opaque = toJSON ([] :: [String])

instance FromJSON NamedTypeDef where
  parseJSON = withObject "TypeDef" $ \v -> toTypeDef <$> v .: "name" <*> v .:? "sumParts" <*> v .:? "prodParts" <*> v .:? "opaque"

toTypeDef :: Identifier -> Maybe [(Identifier, Type)] -> Maybe [(Identifier, Type)] -> Maybe [(Identifier, Type)] -> NamedTypeDef
toTypeDef name (Just x) _ _ = NamedTypeDef name $ SumParts x
toTypeDef name _ (Just x) _ = NamedTypeDef name $ ProdParts x
toTypeDef name _ _ (Just _) = NamedTypeDef name Opaque
toTypeDef _ _ _ _ = error "Unable to parse JSON"

instance FromJSON FunDef

instance ToJSON FunDef

instance FromJSON DefFile

instance ToJSON DefFile

instance FromJSON UnnamedType

instance ToJSON UnnamedType

instance FromJSON Type

instance ToJSON Type

instance FromJSON PrimitiveType

instance ToJSON PrimitiveType
