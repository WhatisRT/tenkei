{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Generators.C where

import Control.Monad
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
  , "import System.IO.Unsafe"
  , "import Pointers"
  , "import Tenkei"
  , ""
  , "foreign import ccall \"tenkei_free\" tenkei_free :: Ptr Word8 -> CSize -> IO ()"
  , ""
  ]

interfaceHeader :: String -> [String]
interfaceHeader libName =
  [ "#ifdef __cplusplus"
  , "extern \"C\" {"
  , "#endif"
  , "  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);"
  , "  extern void hs_init(int* argc, char** argv[]);"
  , "  extern void hs_exit();"
  , "#ifdef __cplusplus"
  , "}"
  , "#endif"
  , ""
  ]

typeId :: Identifier -> String
typeId = snakeCase

functionId :: Identifier -> String
functionId = snakeCase

variableId :: Identifier -> String
variableId = snakeCase

foreignFunctionId :: Identifier -> String
foreignFunctionId = ("tenkei_" ++) . snakeCase

indent :: Int -> [String] -> [String]
indent = indentStr " "

generateCLib :: DefFile -> String
generateCLib = unlines . generateCLib'

generateCLib' :: DefFile -> [String]
generateCLib' (DefFile libName funDefs typeDefs) =
  libHeader (pascalCase libName) ++ (funDefs >>= funDefToImport) ++ (typeDefs >>= typeDefToText)

generateCInterface :: DefFile -> String
generateCInterface = unlines . generateCInterface'

generateCInterface' :: DefFile -> [String]
generateCInterface' (DefFile libName funDefs typeDefs) =
  [ "#ifdef __cplusplus"
  , "extern \"C\" {"
  , "#endif"
  , "  extern void tenkei_free(uint8_t *buffer, size_t buffer_len);"
  , "  extern void hs_init(int* argc, char** argv[]);"
  , "  extern void hs_exit();"
  , ""
  ] ++
  indent 2 (funDefs >>= funDefToLibImport) ++
  ["#ifdef __cplusplus", "}", "#endif", ""] ++
  (typeDefs >>= typeDefToText) ++ (requiredTypes >>= primitiveTypeDefToText ) ++ ["#include \"serializers.c\"", ""] ++ (funDefs >>= funDefToImport)
  where
    types (FunDef _ sources target) = target : fmap snd sources
    requiredTypes = nub $ funDefs >>= types

typeToC :: Type -> String
typeToC (Named ident) = "struct " ++ typeId ident
typeToC (Unnamed (Primitive (List t))) = "struct list_" ++ typeToC' t
typeToC (Unnamed (Any _)) = "void*"
typeToC x = typeToC' x

typeToC' :: Type -> String
typeToC' (Unnamed (Primitive Unit)) = "()"
typeToC' (Unnamed (Primitive Bool)) = "Bool"
typeToC' (Unnamed (Primitive Int8)) = "int8_t"
typeToC' (Unnamed (Primitive Int16)) = "int16_t"
typeToC' (Unnamed (Primitive Int32)) = "int32_t"
typeToC' (Unnamed (Primitive Int64)) = "int64_t"
typeToC' (Unnamed (Primitive UInt8)) = "uint8"
typeToC' (Unnamed (Primitive UInt16)) = "uint16_t"
typeToC' (Unnamed (Primitive UInt32)) = "uint32_t"
typeToC' (Unnamed (Primitive UInt64)) = "uint64_t"
typeToC' (Unnamed (Primitive Float16)) = "Float16"
typeToC' (Unnamed (Primitive Float32)) = "Float32"
typeToC' (Unnamed (Primitive Float64)) = "Float64"
typeToC' (Unnamed (Primitive CodepointUnicode)) = "char"
typeToC' (Unnamed (Primitive StringUTF8)) = "String"
typeToC' (Unnamed (Primitive (Function sources target))) =
  typeToC target ++ "(f*)(" ++ (intercalate ", " $ fmap (\(_,t) -> typeToC t ++ "*") sources) ++ ")"
typeToC' (Unnamed (Primitive (List t))) = "list_" ++ typeToC' t
typeToC' (Unnamed (Any ident)) = "tenkei_ptr"
typeToC' (Named ident) = typeId ident

typeToSerializer :: Type -> String
typeToSerializer (Unnamed (Any _)) = "tenkei_ptr"
typeToSerializer x = typeToC' x

variableToC :: Variable -> String
variableToC (name, (Unnamed (Primitive (Function sources target)))) =
  typeToC target ++ "(" ++ variableId name ++ "*)(" ++ (intercalate ", " $ fmap (\(_,t) -> typeToC t ++ "*") sources) ++ ")"
variableToC (name, t) = typeToC t ++ " " ++ variableId name

mif :: Monoid m => Bool -> m -> m
mif True = id
mif False = const mempty

funDefToExport :: FunDef -> [String]
funDefToExport f@(FunDef name sources _) = undefined

funDefToLibImport :: FunDef -> [String]
funDefToLibImport (FunDef name _ _) =
  ["extern void " ++ foreignFunctionId name ++ "(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);"]

funDefToImport :: FunDef -> [String]
funDefToImport f@(FunDef name sources target) =
  [ typeToC target ++
    " " ++ functionId name ++ "(" ++ intercalate ", " (fmap variableToC sources) ++ ")"
  , "{"
  ] ++
  indent
    2
    (["cbor_item_t *args = cbor_new_definite_array(" ++ show (length sources) ++ ");"] ++
     join (
      zipWith
        (\(n, t) i ->
           [ "cbor_item_t *arg" ++ show i ++ " = serialize_" ++ typeToSerializer t ++ "(" ++ variableId n ++ ");"
           , "cbor_array_push(args, arg" ++ show i ++ ");"
           ])
        sources
        [1 ..]) ++
     [ "cbor_item_t *res = call_cbor(" ++ foreignFunctionId name ++ ", args);"
     , typeToC target ++ " result = deserialize_" ++ typeToSerializer target ++ "(res);"
     , "cbor_decref(&args);"
     , "cbor_decref(&res);"
     , "return result;"
     ]) ++
  ["}", ""]

typeDefToText :: NamedTypeDef -> [String]
typeDefToText = undefined
--typeDefToText (NamedTypeDef name (SumParts parts)) =
--typeDefToText (NamedTypeDef name (ProdParts parts)) =
--typeDefToText (NamedTypeDef name Opaque) = [printf "data %s = Opaque%s Integer" (typeId name) (typeId name)]

primitiveTypeDefToText :: Type -> [String]
primitiveTypeDefToText (Unnamed (Primitive (List t))) =
  ["struct list_" ++ typeToC' t ++ " {", "  " ++ typeName ++ " *start;", "  unsigned int length;", "};", ""]
  where typeName = case t of
          Unnamed (Any _) -> "void *"
          _ -> typeToC' t
primitiveTypeDefToText _ = []
