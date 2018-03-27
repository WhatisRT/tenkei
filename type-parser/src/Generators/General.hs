module Generators.General where

import Control.Monad
import Data.Char
import Data.List

import Types

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
pascalCase = join . fmap title
