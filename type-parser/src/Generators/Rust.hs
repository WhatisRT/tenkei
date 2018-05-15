{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Generators.Rust where

import Data.List
import Generators.General
import Text.Printf
import Types

indent :: Int -> [String] -> [String]
indent = indentStr "    "

header :: [String]
header =
  [ "extern crate serde;"
  , "extern crate serde_cbor as cbor;"
  , "#[macro_use]"
  , "extern crate serde_derive;"
  , ""
  , "use std::ops;"
  , "use std::ptr;"
  , "use std::slice;"
  , ""
  , "#[derive(Deserialize, Serialize)]"
  , "struct Any(usize);"
  , ""
  , "struct Function(fn("
  , "    input: *const u8,"
  , "    input_len: usize,"
  , "    output: *mut *mut u8,"
  , "    output_len: *mut usize,"
  , "));"
  , ""
  , "struct Buffer {"
  , "    ptr: *mut u8,"
  , "    len: usize,"
  , "}"
  , ""
  , "impl Buffer {"
  , "    fn new() -> Buffer {"
  , "        Buffer {"
  , "            ptr: ptr::null_mut(),"
  , "            len: 0,"
  , "        }"
  , "    }"
  , "}"
  , ""
  , "impl ops::Deref for Buffer {"
  , "    type Target = [u8];"
  , "    fn deref(&self) -> &[u8] {"
  , "        unsafe {"
  , "            slice::from_raw_parts(self.ptr, self.len)"
  , "        }"
  , "    }"
  , "}"
  , ""
  , "impl Drop for Buffer {"
  , "    fn drop(&mut self) {"
  , "        unsafe {"
  , "            sys::tenkei_free(self.ptr, self.len);"
  , "        }"
  , "    }"
  , "}"
  ]

createRustFile :: DefFile -> String
createRustFile = unlines . generate

generate :: DefFile -> [String]
generate (DefFile _ functions types) = header ++ (types >>= typeDef) ++ foreignFunctionDefs functions ++ (functions >>= functionDef)

primitive :: PrimitiveType -> String
primitive Unit = "()"
primitive Bool = "bool"
primitive Int8 = "i8"
primitive Int16 = "i16"
primitive Int32 = "i32"
primitive Int64 = "i64"
primitive UInt8 = "u8"
primitive UInt16 = "u16"
primitive UInt32 = "u32"
primitive UInt64 = "u64"
primitive Float16 = "f32"
primitive Float32 = "f32"
primitive Float64 = "f64"
primitive CodepointUnicode = "char"
primitive StringUTF8 = "String"
primitive (List t) = printf "Vec<%s>" $ typeIdent t
primitive (Function _ _) = "Function"

namedTypeIdent :: Identifier -> String
foreignFunctionIdent :: Identifier -> String
functionIdent :: Identifier -> String
variableIdent :: Identifier -> String

namedTypeIdent = pascalCase
foreignFunctionIdent = snakeCase
functionIdent = snakeCase
variableIdent = snakeCase

typeIdent :: Type -> String
typeIdent (Named c args) = namedTypeIdent c
typeIdent (Unnamed (Primitive p)) = primitive p
typeIdent (Unnamed (Any _)) = "Any"

variable :: Variable -> String
variable (ident, type_) = printf "%s: %s" (variableIdent ident) (typeIdent type_)

foreignFunctionsDefsHeader :: [String]
foreignFunctionsDefsHeader = ["mod sys {", "    extern \"C\" {"]

foreignFunctionDef :: FunDef -> [String]
foreignFunctionDef (FunDef name _ _) =
  [ printf "pub fn %s(" $ foreignFunctionIdent name
  , "    input: *const u8,"
  , "    input_len: usize,"
  , "    output: *mut *mut u8,"
  , "    output_len: *mut usize,"
  , ");"
  ]

foreignFunctionsDefsFooter :: [String]
foreignFunctionsDefsFooter = ["        pub fn tenkei_free(buffer: *mut u8, buffer_len: usize);", "    }", "}"]

foreignFunctionDefs :: [FunDef] -> [String]
foreignFunctionDefs funDefs = foreignFunctionsDefsHeader ++ indent 2 (funDefs >>= foreignFunctionDef) ++ foreignFunctionsDefsFooter

argumentSerialization :: [Variable] -> String
argumentSerialization [(var, _)] = variableIdent var
argumentSerialization vars = printf "(%s)" $ intercalate ", " (fmap (variableIdent . fst) vars)

functionDef :: FunDef -> [String]
functionDef (FunDef name sources target) =
  [ printf "pub fn %s(%s) -> %s {" (functionIdent name) (intercalate ", " (fmap variable sources)) (typeIdent target)
  , "    fn wrapper(input: &[u8]) -> Buffer {"
  , "        let mut buffer = Buffer::new();"
  , "        unsafe {"
  , printf "            sys::%s(input.as_ptr(), input.len(), &mut buffer.ptr, &mut buffer.len);" $ foreignFunctionIdent name
  , "        }"
  , "        buffer"
  , "    }"
  , printf "    cbor::from_slice(&wrapper(&cbor::to_vec(&%s).unwrap())).unwrap()" $ argumentSerialization sources
  , "}"
 ]

typeHeader :: [String]
typeHeader = ["#[derive(Deserialize, Serialize)]"]

typeDefImpl :: NamedTypeDef -> [String]
typeDefImpl (NamedTypeDef name args (SumParts parts)) =
  [printf "pub enum %s {" $ namedTypeIdent name] ++ fmap (\(n, t) -> printf "    %s(%s)," (namedTypeIdent n) (typeIdent t)) parts ++ ["}"]
typeDefImpl (NamedTypeDef name args (ProdParts parts)) =
  [printf "pub struct %s {" $ namedTypeIdent name] ++ fmap (\(n, t) -> printf "    %s," (variable (n, t))) parts ++ ["}"]
typeDefImpl (NamedTypeDef name args Opaque) = [printf "pub struct %s(usize);" $ namedTypeIdent name]

typeDef :: NamedTypeDef -> [String]
typeDef = (typeHeader ++) . typeDefImpl
