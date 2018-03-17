{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Char
import Data.Foldable
import Data.List

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (writeFile)

import GHC.Generics

type Identifier = [String]
data TypeDef = TypeDef { typeName :: Identifier, parts :: TypeParts } deriving Show
data TypeParts = SumParts [(Identifier,Type)] | ProdParts [(Identifier,Type)] | Unit deriving Show
data Type = Primitive PrimitiveType | Composite Identifier deriving (Generic, Show)
data PrimitiveType = Int32 deriving (Generic, Show)

data FunDef = FunDef { funName :: Identifier, source :: Type, target :: Type } deriving (Generic, Show)

data DefFile = DefFile { funDefs :: [FunDef], typeDefs :: [TypeDef] } deriving (Generic, Show)

decodeType :: String -> Maybe DefFile
decodeType = decode . pack

toTypeDef :: Identifier -> Maybe [(Identifier,Type)] -> Maybe [(Identifier,Type)] -> TypeDef
toTypeDef name (Just x) _ = TypeDef name $ SumParts x
toTypeDef name _ (Just x) = TypeDef name $ ProdParts x
toTypeDef name _ _ = TypeDef name Unit

instance ToJSON TypeDef where
  toJSON x = object ["name" .= typeName x, partsType .= parts x]
    where partsType = case (parts x) of
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

instance FromJSON FunDef
instance ToJSON FunDef

instance FromJSON DefFile
instance ToJSON DefFile

instance FromJSON Type
instance ToJSON Type

instance FromJSON PrimitiveType
instance ToJSON PrimitiveType


writeDefFile :: FilePath -> DefFile -> IO ()
writeDefFile path = Data.ByteString.Lazy.writeFile path . encodePretty

title :: String -> String
title (c:cs) = toUpper c : cs
title [] = []

upper :: String -> String
upper = fmap toUpper

camelCase :: Identifier -> String
camelCase (n:ns) = n ++ pascalCase ns
camelCase [] = []

snakeCase :: Identifier -> String
snakeCase = intercalate "_"

screamingSnakeCase :: Identifier -> String
screamingSnakeCase = intercalate "_" . fmap upper

pascalCase :: Identifier -> String
pascalCase = fold . fmap title
