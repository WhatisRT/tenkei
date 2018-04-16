{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Generators.C (generateCInterface, generateCLib) where

import Control.Monad
import Data.List
import Generators.General
import Types

typeId :: Identifier -> String
typeId = snakeCase

functionId :: Identifier -> String
functionId = snakeCase

variableId :: Identifier -> String
variableId = snakeCase

foreignFunctionId :: Identifier -> String
foreignFunctionId = ("tenkei_" ++) . snakeCase

indent :: Int -> [String] -> [String]
indent = indentStr "  "

generateCLib :: DefFile -> String
generateCLib = unlines . generateCLib'

generateCLib' :: DefFile -> [String]
generateCLib' (DefFile libName funDefs _) =
  [ "#include \"../libtenkei-c/ffi_wrappers.h\""
  , "#include \"" ++ snakeCase libName ++ ".h\""
  , "#include \"../libtenkei-c/serializers.h\""
  , "#include \"../common/list_serializers.h\""
  , ""
  , "void tenkei_free(uint8_t *arg, size_t len)"
  , "{"
  , "  return;"
  , "}"
  , ""
  ] ++
  (funDefs >>= funDefToExport)

generateCInterface :: DefFile -> String
generateCInterface = unlines . generateCInterface'

generateCInterface' :: DefFile -> [String]
generateCInterface' (DefFile _ funDefs typeDefs) =
  [ "#include \"../libtenkei-c/ffi_wrappers.h\""
  , "#include \"../common/list_serializers.h\""
  , ""
  , "void tenkei_free(uint8_t *buffer, size_t buffer_len);"
  , ""
  ] ++
  (funDefs >>= funDefToLibImport) ++
  (typeDefs >>= typeDefToText) -- ++ (requiredTypes >>= primitiveTypeDefToText )
  ++ (funDefs >>= funDefToImport)
  where
    types (FunDef _ sources target) = target : fmap snd sources
    requiredTypes = nub $ funDefs >>= types

typeToC :: Type -> String
typeToC (Named ident) = "struct " ++ typeId ident
typeToC (Unnamed (Primitive (List t))) = "struct list_" ++ typeToC' t
typeToC (Unnamed (Any _)) = "struct tenkei_value"
typeToC x = typeToC' x

typeToC' :: Type -> String
typeToC' (Unnamed (Primitive Unit)) = "()"
typeToC' (Unnamed (Primitive Bool)) = "bool"
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
  typeToC target ++ "(f*)(" ++ intercalate ", " (fmap (\(_,t) -> typeToC t ++ "*") sources) ++ ")"
typeToC' (Unnamed (Primitive (List t))) = "list_" ++ typeToC' t
typeToC' (Unnamed (Any _)) = "tenkei_value"
typeToC' (Named ident) = typeId ident

typeToSerializer :: Type -> String
typeToSerializer (Unnamed (Primitive (Function sources target))) = "fun_ptr"
typeToSerializer (Unnamed (Any _)) = "tenkei_value"
typeToSerializer x = typeToC' x

variableToC :: Variable -> String
variableToC (name, Unnamed (Primitive (Function sources target))) =
  "void (*" ++ variableId name ++ ")(uint8_t *, size_t, uint8_t **, size_t *)"
  -- typeToC target ++ "(*" ++ variableId name ++ ")(" ++ intercalate ", " (fmap (\(_,t) -> typeToC t) sources) ++ ")"
variableToC (name, t) = typeToC t ++ " " ++ variableId name

funDefToExport :: FunDef -> [String]
funDefToExport (FunDef name sources target) =
  ["cbor_item_t *cbor_" ++ functionId name ++ "(cbor_item_t *args)", "{"] ++
  indent 1
    (["cbor_item_t **arg_list = cbor_array_handle(args);"] ++
     zipWith deserializeArg ([0 ..] :: [Int]) (fmap snd sources) ++
     [ typeToC target ++ " res = " ++ functionId name ++ "(" ++ intercalate ", " argList ++ ");"
     , "cbor_item_t *result = serialize_" ++ typeToSerializer target ++ "(res);"
     , "return result;"
     ]) ++
  ["};", "", "void tenkei_" ++ functionId name ++ "(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)", "{"] ++
  indent 1 ["offer_cbor(cbor_" ++ functionId name ++ ", input, input_len, output, output_len);"] ++ ["}", ""]
  where
    argList = fmap (("arg" ++) . show) [0 .. length sources - 1]
    deserializeArg index argType =
      case argType of
        Unnamed (Primitive (Function sources target)) -> "void (*arg" ++ show index ++ ")(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len) = deserialize_fun_ptr(arg_list[" ++ show index ++ "]);"
        _ -> typeToC argType ++ " arg" ++ show index ++ " = deserialize_" ++ typeToSerializer argType ++ "(arg_list[" ++ show index ++ "]);"

funDefToLibImport :: FunDef -> [String]
funDefToLibImport (FunDef name _ _) =
  ["extern void " ++ foreignFunctionId name ++ "(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);"]

funDefToImport :: FunDef -> [String]
funDefToImport (FunDef name sources target) =
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
        ([1 ..] :: [Int])) ++
     [ "cbor_item_t *res = call_cbor(" ++ foreignFunctionId name ++ ", args);"
     , typeToC target ++ " result = deserialize_" ++ typeToSerializer target ++ "(res);"
     , "cbor_decref(&args);"
     --, "cbor_decref(&res);"
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
          Unnamed (Any _) -> "struct tenkei_value"
          _ -> typeToC' t
primitiveTypeDefToText _ = []
