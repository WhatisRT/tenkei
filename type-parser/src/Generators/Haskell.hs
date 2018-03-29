{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Generators.Haskell where

import Data.List
import Generators.General
import Text.Printf
import Types

libHeader :: String -> [String]
libHeader libName =
  [ "{-# LANGUAGE ForeignFunctionInterface #-}"
  , ""
  , "module " ++ libName ++ " where"
  , ""
  , "import Foreign"
  , "import Foreign.C"
  , ""
  , "import Data.CBOR"
  , "import FFIWrappers"
  , "import Tenkei"
  , ""
  , "foreign import ccall \"tenkei_free\" tenkei_free :: Ptr Word8 -> CSize -> IO ()"
  , ""
  ]

interfaceHeader :: String -> [String]
interfaceHeader libName =
  [ "{-# LANGUAGE ForeignFunctionInterface #-}"
  , ""
  , "module " ++ libName ++ "Tenkei where"
  , ""
  , "import Foreign"
  , "import Foreign.C"
  , ""
  , "import Data.CBOR"
  , "import FFIWrappers"
  , "import Tenkei"
  , "import " ++ libName
  , ""
  , "tenkei_free :: Ptr Word8 -> CSize -> IO ()"
  , "tenkei_free = tenkeiFree"
  , "foreign export ccall tenkei_free :: Ptr Word8 -> CSize -> IO ()"
  , ""
  ]

typeId :: Identifier -> String
typeId = pascalCase

functionId :: Identifier -> String
functionId = camelCase

foreignFunctionId :: Identifier -> String
foreignFunctionId = ("tenkei_" ++) . snakeCase

generateHaskellLib :: DefFile -> String
generateHaskellLib = intercalate "\n" . generateHaskellLib'

generateHaskellLib' :: DefFile -> [String]
generateHaskellLib' (DefFile libName funDefs typeDefs) =
  libHeader (pascalCase libName) ++ (funDefs >>= funDefToText) ++ (typeDefs >>= typeDefToText)

generateHaskellInterface :: DefFile -> String
generateHaskellInterface = intercalate "\n" . generateHaskellInterface'

generateHaskellInterface' :: DefFile -> [String]
generateHaskellInterface' (DefFile libName funDefs _) = interfaceHeader (pascalCase libName) ++ (funDefs >>= funDefToExport)

typeToHaskell :: Type -> String
typeToHaskell (Primitive Int32) = "Int32"
typeToHaskell (Primitive Int64) = "Int64"
typeToHaskell (Primitive CodepointUnicode) = "Char"
typeToHaskell (Primitive (List t)) = printf "[%s]" $ typeToHaskell t
typeToHaskell (Composite ident) = pascalCase ident

funDefToExport :: FunDef -> [String]
funDefToExport (FunDef name sources _) =
  [ printf "%s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()" $ foreignFunctionId name
  , funImpl
  , printf "foreign export ccall %s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()" $ foreignFunctionId name
  , ""
  ]
  where
    argList = fmap (("arg" ++) . show) [1..length sources]
    funImpl = case length sources of
      1 -> printf "%s = offer %s" (foreignFunctionId name) $ functionId name
      _ -> printf "%s = offerCBOR (\\(CBOR_Array [%s]) -> serialize $ %s %s)" (foreignFunctionId name) (intercalate ", " argList) (functionId name) (unwords $ fmap (\s -> "(deserialize " ++ s ++ ")") argList)

funDefToText :: FunDef -> [String]
funDefToText (FunDef name sources target) =
  [ printf
      "foreign import ccall \"%s\" foreign_%s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()"
      (foreignFunctionId name)
      (foreignFunctionId name)
  , printf "%s :: %s -> %s" (functionId name) (intercalate " -> " $ fmap typeToHaskell sources) (typeToHaskell target)
  , funImpl
  , ""
  ]
  where
    argList = fmap (("arg" ++) . show) [1..length sources]
    funImpl = case length sources of
          1 -> printf "%s = call foreign_%s tenkei_free" (functionId name) (foreignFunctionId name)
          _ -> printf "%s %s = deserialize $ callCBOR foreign_%s tenkei_free $ CBOR_Array [%s]" (functionId name) (unwords argList) (foreignFunctionId name) $ intercalate ", " $ fmap ("serialize " ++) argList

typeDefToText :: TypeDef -> [String]
typeDefToText (TypeDef name (SumParts parts)) =
  [printf "data %s = %s" (typeId name) $ intercalate " | " $ fmap (\(n, t) -> typeId n ++ " " ++ typeToHaskell t) parts]
typeDefToText (TypeDef name (ProdParts parts)) =
  [ printf "data %s = %s { %s }" (typeId name) (typeId name) $
    intercalate ", " $ fmap (\(n, t) -> functionId n ++ " :: " ++ typeToHaskell t) parts
  ]
typeDefToText (TypeDef name Unit) = [printf "data %s = Unit" (typeId name)]
