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
  , "import System.IO.Unsafe"
  , ""
  , "import FFIWrappers"
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
  , "import FFIWrappers"
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
typeToHaskell (Primitive Char) = "Char"
typeToHaskell (Primitive (Array t)) = printf "[%s]" $ typeToHaskell t
typeToHaskell (Composite ident) = pascalCase ident

funDefToExport :: FunDef -> [String]
funDefToExport (FunDef name _ _) =
  [ printf "%s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()" $ foreignFunctionId name
  , printf "%s = offer %s" (foreignFunctionId name) $ functionId name
  , printf "foreign export ccall %s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()" $ foreignFunctionId name
  , ""
  ]

funDefToText :: FunDef -> [String]
funDefToText (FunDef name source target) =
  [ printf
      "foreign import ccall \"%s\" foreign_%s :: Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()"
      (foreignFunctionId name)
      (foreignFunctionId name)
  , printf "%s :: %s -> %s" (functionId name) (typeToHaskell source) (typeToHaskell target)
  , printf "%s = call foreign_%s tenkei_free" (functionId name) (foreignFunctionId name)
  , ""
  ]

typeDefToText :: TypeDef -> [String]
typeDefToText (TypeDef name (SumParts parts)) =
  [printf "data %s = %s" (typeId name) $ intercalate " | " $ fmap (\(n, t) -> typeId n ++ " " ++ typeToHaskell t) parts]
typeDefToText (TypeDef name (ProdParts parts)) =
  [ printf "data %s = %s { %s }" (typeId name) (typeId name) $
    intercalate ", " $ fmap (\(n, t) -> functionId n ++ " :: " ++ typeToHaskell t) parts
  ]
typeDefToText (TypeDef name Unit) = [printf "data %s = Unit" (typeId name)]
