{-# OPTIONS_GHC -Wall -Wno-name-shadowing #-}
{-# LANGUAGE OverloadedStrings #-}

module Generators.Rust where

import Control.Monad
import Generators.General
import Text.Printf
import Types

indent :: Int -> [String] -> [String]
indent i = fmap $ (++) (join (replicate i "    "))

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
createRustFile = join . fmap (++ "\n") . generate

generate :: DefFile -> [String]
generate (DefFile _ functions types) = header ++ (types >>= typeDef) ++ foreignFunctionDefs functions ++ (functions >>= functionDef)

primitive :: PrimitiveType -> String
primitive Int32 = "i32"

composite :: Identifier -> String
foreignFunction :: Identifier -> String
function :: Identifier -> String
member :: Identifier -> String
variable :: Identifier -> String
composite = pascalCase

foreignFunction = snakeCase

function = snakeCase

member = snakeCase

variable = snakeCase

type_ :: Type -> String
type_ (Primitive p) = primitive p
type_ (Composite c) = composite c

foreignFunctionsDefsHeader :: [String]
foreignFunctionsDefsHeader = ["mod sys {", "    extern \"C\" {"]

foreignFunctionDef :: FunDef -> [String]
foreignFunctionDef (FunDef name _ _) =
  [ printf "pub fn %s(" $ foreignFunction name
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

functionDef :: FunDef -> [String]
functionDef (FunDef name [source] target) =
  [ printf "pub fn %s(x: %s) -> %s {" (function name) (type_ source) (type_ target)
  , "    fn wrapper(input: &[u8]) -> Buffer {"
  , "        let mut buffer = Buffer::new();"
  , "        unsafe {"
  , printf "            sys::%s(input.as_ptr(), input.len(), &mut buffer.ptr, &mut buffer.len);" $ foreignFunction name
  , "        }"
  , "        buffer"
  , "    }"
  , "    cbor::from_slice(&wrapper(&cbor::to_vec(&x).unwrap())).unwrap()"
  , "}"
  ]

typeHeader :: [String]
typeHeader = ["#[derive(Deserialize, Serialize)]"]

typeDefImpl :: TypeDef -> [String]
typeDefImpl (TypeDef name (SumParts parts)) =
  [printf "pub enum %s {" $ composite name] ++ fmap (\(n, t) -> printf "    %s(%s)," (composite n) (type_ t)) parts ++ ["}"]
typeDefImpl (TypeDef name (ProdParts parts)) =
  [printf "pub struct %s {" $ composite name] ++ fmap (\(n, t) -> printf "    %s: %s," (member n) (type_ t)) parts ++ ["}"]
typeDefImpl (TypeDef name Unit) = [printf "struct %s;" $ composite name]

typeDef :: TypeDef -> [String]
typeDef = (typeHeader ++) . typeDefImpl
