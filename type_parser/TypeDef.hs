{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module TypeDef where

import Data.Char
import Data.Foldable
import Data.List

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)

type Identifier = [String]
data TypeDef = TypeDef { name :: String, parts :: TypeParts } deriving Show
data TypeParts = SumParts [(String,String)] | ProdParts [String] | Unit deriving Show

decodeType :: String -> Maybe TypeDef
decodeType = decode . pack

toTypeDef :: String -> Maybe [(String,String)] -> Maybe [String] -> TypeDef
toTypeDef n (Just x) _ = TypeDef n $ SumParts x
toTypeDef n _ (Just x) = TypeDef n $ ProdParts x
toTypeDef n _ _ = TypeDef n Unit

instance ToJSON TypeDef where
  toJSON x = object ["name" .= name x, partsType .= parts x]
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
