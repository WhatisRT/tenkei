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

hasTypeVar :: Type -> Bool
hasTypeVar (Unnamed (Any _)) = True
hasTypeVar (Unnamed (Primitive (List a))) = hasTypeVar a
hasTypeVar _ = False

getTypeVars :: Type -> [Identifier]
getTypeVars (Unnamed (Primitive (Function sources target))) = (fmap snd sources ++ [target]) >>= getTypeVars
getTypeVars (Unnamed (Primitive (List t))) = getTypeVars t
getTypeVars (Unnamed (Any ident)) = [ident]
getTypeVars _ = []

hasTypeVars :: FunDef -> Bool
hasTypeVars (FunDef _ sources target) = any hasTypeVar (target : fmap snd sources)
