{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Generators.Cpp (cppLanguageGenerators) where

import Control.Monad
import Data.List
import Generators.General
import Types
import LanguageFunctions

cppLanguageGenerators :: LanguageGenerators
cppLanguageGenerators = LanguageGenerators generateCppLib generateCppInterface []

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

generateCppLib :: DefFile -> String
generateCppLib = unlines . generateCppLib'

generateCppLib' :: DefFile -> [String]
generateCppLib' (DefFile libName funDefs _) =
  [ "#include \"../libtenkei-cpp/ffi_wrappers.h\""
  , "#include \"" ++ snakeCase libName ++ ".h\""
  , "#include \"../libtenkei-cpp/serializers.h\""
  , ""
  , "extern \"C\" void tenkei_free(uint8_t *arg, size_t len)"
  , "{"
  , "  return;"
  , "}"
  , ""
  ] ++
  (funDefs >>= funDefToExport)

generateCppInterface :: DefFile -> String
generateCppInterface = unlines . generateCppInterface'

generateCppInterface' :: DefFile -> [String]
generateCppInterface' (DefFile _ funDefs typeDefs) =
  [ "#include \"../libtenkei-cpp/ffi_wrappers.h\""
  , ""
  , "void tenkei_free(uint8_t *buffer, size_t buffer_len);"
  , ""
  ] ++
  (funDefs >>= funDefToLibImport) ++
  [""] ++
  (typeDefs >>= typeDefToText) -- ++ (requiredTypes >>= primitiveTypeDefToText )
   ++
  (funDefs >>= funDefToImport)
  where
    types (FunDef _ sources target) = target : fmap snd sources
    requiredTypes = nub $ funDefs >>= types

typeToCpp :: Type -> String
typeToCpp (Named ident) = "struct " ++ typeId ident
typeToCpp t@(Unnamed (Primitive (Function _ _))) = "struct " ++ typeToCpp' t
typeToCpp (Unnamed (Any _)) = "struct tenkei_value"
typeToCpp x = typeToCpp' x

typeToCpp' :: Type -> String
typeToCpp' (Unnamed (Primitive Unit)) = "()"
typeToCpp' (Unnamed (Primitive Bool)) = "bool"
typeToCpp' (Unnamed (Primitive Int8)) = "int8_t"
typeToCpp' (Unnamed (Primitive Int16)) = "int16_t"
typeToCpp' (Unnamed (Primitive Int32)) = "int32_t"
typeToCpp' (Unnamed (Primitive Int64)) = "int64_t"
typeToCpp' (Unnamed (Primitive UInt8)) = "uint8"
typeToCpp' (Unnamed (Primitive UInt16)) = "uint16_t"
typeToCpp' (Unnamed (Primitive UInt32)) = "uint32_t"
typeToCpp' (Unnamed (Primitive UInt64)) = "uint64_t"
typeToCpp' (Unnamed (Primitive Float16)) = "Float16"
typeToCpp' (Unnamed (Primitive Float32)) = "Float32"
typeToCpp' (Unnamed (Primitive Float64)) = "Float64"
typeToCpp' (Unnamed (Primitive CodepointUnicode)) = "uint32_t"
typeToCpp' (Unnamed (Primitive StringUTF8)) = "String"
typeToCpp' (Unnamed (Primitive (List t))) = "std::vector<" ++ typeToCpp' t ++ ">"
typeToCpp' (Unnamed (Primitive (Function _ _))) = "tenkei_fun_ptr"
typeToCpp' (Unnamed (Any _)) = "tenkei_value"
typeToCpp' (Named ident) = typeId ident

typeToSerializer :: Type -> String
typeToSerializer = typeToCpp'

variableToCpp :: Variable -> String
-- variableToCpp (name, Unnamed (Primitive (Function _ _))) =
--   "void (*" ++ variableId name ++ ")(uint8_t *, size_t, uint8_t **, size_t *)"
--   -- typeToCpp target ++ "(*" ++ variableId name ++ ")(" ++ intercalate ", " (fmap (\(_,t) -> typeToCpp t) sources) ++ ")"
variableToCpp (name, t) = typeToCpp t ++ " " ++ variableId name

funDefToExport :: FunDef -> [String]
funDefToExport (FunDef name sources target) =
  ["cbor_item_t *cbor_" ++ functionId name ++ "(cbor_item_t *args)", "{"] ++
  indent
    1
    (["cbor_item_t **arg_list = cbor_array_handle(args);"] ++
     zipWith deserializeArg ([0 ..] :: [Int]) (fmap snd sources) ++
     [ typeToCpp target ++ " res = " ++ functionId name ++ funTemplates ++ "(" ++ intercalate ", " argList ++ ");"
     , "cbor_item_t *result = serialize(res);"
     , "return result;"
     ]) ++
  [ "};"
  , ""
  , "extern \"C\" void tenkei_" ++ functionId name ++ "(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len)"
  , "{"
  ] ++
  indent 1 ["offer_cbor(cbor_" ++ functionId name ++ ", input, input_len, output, output_len);"] ++ ["}", ""]
  where
    typeVariables = nub $ (target : fmap snd sources) >>= getTypeVars
    funTemplates =
      if null typeVariables
        then []
        else "<" ++ intercalate ", " (typeVariables >> ["tenkei_value"]) ++ ">"
    argList = fmap (("arg" ++) . show) [0 .. length sources - 1]
    deserializeArg index argType =
      typeToCpp argType ++ " arg" ++ show index ++ " = deserialize<" ++ typeToSerializer argType ++ ">(arg_list[" ++ show index ++ "]);"

funDefToLibImport :: FunDef -> [String]
funDefToLibImport (FunDef name _ _) =
  ["extern \"C\" void " ++ foreignFunctionId name ++ "(uint8_t *input, size_t input_len, uint8_t **output, size_t *output_len);"]

funDefToImport :: FunDef -> [String]
funDefToImport (FunDef name sources target) =
  [ typeToCpp target ++
    " " ++ functionId name ++ "(" ++ intercalate ", " (fmap variableToCpp sources) ++ ")"
  , "{"
  ] ++
  indent 1
    (["cbor_item_t *args = cbor_new_definite_array(" ++ show (length sources) ++ ");"] ++
     join (
      zipWith
        (\(n, t) i ->
           [ "cbor_item_t *arg" ++ show i ++ " = serialize<" ++ typeToSerializer t ++ ">(" ++ variableId n ++ ");"
           , "cbor_array_push(args, arg" ++ show i ++ ");"
           ])
        sources
        ([1 ..] :: [Int])) ++
     [ "cbor_item_t *res = call_cbor(" ++ foreignFunctionId name ++ ", args);"
     , typeToCpp target ++ " result = deserialize<" ++ typeToSerializer target ++ ">(res);"
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
