{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module TypeDef where

import Data.Char
import Data.Foldable
import Data.List

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Lazy (writeFile)

import GHC.Generics

module Types where

type Identifier = [String]
data TypeDef = TypeDef { typeName :: String, parts :: TypeParts } deriving Show
data TypeParts = SumParts [(String,String)] | ProdParts [String] | Unit deriving Show
primitiveTypes = ["Int8","Int16","Int32","Int64","String"] :: [String] -- to specify a primitive type definition, use Unit

data FunDef = FunDef { funName :: String, source :: String, target :: String } deriving (Generic, Show)

data DefFile = DefFile { funDefs :: [FunDef], typeDefs :: [TypeDef] } deriving (Generic, Show)

decodeType :: String -> Maybe TypeDef
decodeType = decode . pack

toTypeDef :: String -> Maybe [(String,String)] -> Maybe [String] -> TypeDef
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
snakeCase = fold . intersperse "_"

screamingSnakeCase :: Identifier -> String
screamingSnakeCase = fold . intersperse "_" . fmap upper

pascalCase :: Identifier -> String
pascalCase = fold . fmap title
