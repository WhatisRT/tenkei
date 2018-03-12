{-# OPTIONS_GHC -Wall #-}

module TypeDef where

import Data.Char
import Data.Foldable
import Data.List

type Identifier = [String]
data TypeDef =
      TypeDefSum { name :: String, sumParts :: [(String,String)] }
    | TypeDefProduct { name :: String, prodParts :: [String] }
    deriving Show

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
