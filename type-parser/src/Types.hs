{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types (Identifier, TypeDef(..), TypeParts(..), Type(..), PrimitiveType(..), FunDef(..), DefFile(..), decodeType, writeDefFile) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (writeFile)
import Prelude hiding (writeFile)

import GHC.Generics

type Identifier = [String]
data TypeDef = TypeDef { typeName :: Identifier, parts :: TypeParts } deriving (Eq, Show)
data TypeParts = SumParts [(Identifier,Type)] | ProdParts [(Identifier,Type)] | Unit deriving (Eq, Show)
data Type = Primitive PrimitiveType | Composite Identifier deriving (Eq, Generic, Show)
data PrimitiveType = Int32 | Int64 | Char | Array Type deriving (Eq, Generic, Show)

data FunDef = FunDef { funName :: Identifier, source :: Type, target :: Type } deriving (Eq, Generic, Show)

data DefFile = DefFile { libName :: Identifier, funDefs :: [FunDef], typeDefs :: [TypeDef] } deriving (Eq, Generic, Show)

decodeType :: String -> Maybe DefFile
decodeType = decode . pack

writeDefFile :: FilePath -> DefFile -> IO ()
writeDefFile path = writeFile path . encodePretty

instance ToJSON TypeDef where
  toJSON x = object ["name" .= typeName x, partsType .= parts x]
    where partsType = case parts x of
            SumParts _ -> "sumParts"
            ProdParts _ -> "prodParts"
            Unit -> "prodParts"

instance ToJSON TypeParts where
  toJSON (SumParts x) = toJSON x
  toJSON (ProdParts x) = toJSON x
  toJSON Unit = toJSON ([] :: [String])

instance FromJSON TypeDef where
  parseJSON = withObject "TypeDef" $ \v ->
    toTypeDef <$> v .: "name" <*> v .:? "sumParts" <*> v .:? "prodParts"

toTypeDef :: Identifier -> Maybe [(Identifier,Type)] -> Maybe [(Identifier,Type)] -> TypeDef
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
