{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}


import Text.Printf
import Data.List
import Types


createHaskellFile :: DefFile -> String
createHaskellFile (DefFile funDefs typeDefs) = printf "%s\n\n\n%s\n"
  (intercalate "\n\n" $ fmap funDefToText funDefs)
  (intercalate "\n" $ fmap typeDefToText typeDefs)

typeToHaskell :: Type -> String
typeToHaskell (Primitive Int32) = "Int32"
typeToHaskell (Composite ident) = camelCase ident

funDefToText :: FunDef -> String
funDefToText (FunDef name source target) = intercalate "\n" [printf
  "foreign import ccall \"%s\" foreign_%s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()"
  (snakeCase name)  (snakeCase name),
  printf "%s :: %s -> %s" (camelCase name) (typeToHaskell source) (typeToHaskell target),
  printf "%s = (fmap deserialize) . (call foreign_%s) . serialize" (camelCase name) (snakeCase name)]

typeDefToText :: TypeDef -> String
typeDefToText (TypeDef name (SumParts parts)) = printf "data %s = %s" (pascalCase name) $ intercalate " | " $ fmap (\(n,t) -> pascalCase n ++ " " ++ typeToHaskell t) parts
typeDefToText (TypeDef name (ProdParts parts)) = printf "data %s = %s { %s }" (pascalCase name) (pascalCase name) $  intercalate ", " $ fmap (\(n,t) -> camelCase n ++ " :: " ++ typeToHaskell t) parts
typeDefToText (TypeDef name Unit) = printf "data %s = Unit" (pascalCase name)
