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

indentStr :: String -> Int -> [String] -> [String]
indentStr str i = fmap $ (++) (join (replicate i str))

isTypeVar :: Type -> Bool
isTypeVar (Unnamed (Any _)) = True
isTypeVar _ = False

hasTypeVars :: FunDef -> Bool
hasTypeVars (FunDef _ sources target) = any isTypeVar (target : fmap snd sources)
