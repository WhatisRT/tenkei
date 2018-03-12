module TypeDef where

type TypeDef = TypeDefSum { name :: String, parts :: [(String,String)] } | TypeDefProduct { name :: String, parts :: [String] }
