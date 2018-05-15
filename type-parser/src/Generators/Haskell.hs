{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Generators.Haskell (haskellLanguageGenerators) where

import Control.Monad
import Data.List
import Generators.General
import Text.Printf
import Types
import LanguageFunctions

haskellLanguageGenerators :: LanguageGenerators
haskellLanguageGenerators = LanguageGenerators generateHaskellLib generateHaskellInterface []

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
  [ "{-# LANGUAGE ForeignFunctionInterface #-}"
  , ""
  , "module " ++ libName ++ "Tenkei where"
  , ""
  , "import Foreign"
  , "import Foreign.C"
  , ""
  , "import Data.CBOR"
  , "import FFIWrappers"
  , "import Pointers"
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

generateHaskellInterface :: DefFile -> String
generateHaskellInterface = unlines . generateHaskellInterface'

generateHaskellInterface' :: DefFile -> [String]
generateHaskellInterface' (DefFile libName funDefs typeDefs) =
  libHeader (pascalCase libName) ++ (funDefs >>= funDefToImport) ++ (typeDefs >>= typeDefToText)

generateHaskellLib :: DefFile -> String
generateHaskellLib = unlines . generateHaskellLib'

generateHaskellLib' :: DefFile -> [String]
generateHaskellLib' (DefFile libName funDefs _) = interfaceHeader (pascalCase libName) ++ (funDefs >>= funDefToExport)

typeToHaskell :: Type -> String
typeToHaskell (Unnamed (Primitive Unit)) = "()"
typeToHaskell (Unnamed (Primitive Bool)) = "Bool"
typeToHaskell (Unnamed (Primitive Int8)) = "Int8"
typeToHaskell (Unnamed (Primitive Int16)) = "Int16"
typeToHaskell (Unnamed (Primitive Int32)) = "Int32"
typeToHaskell (Unnamed (Primitive Int64)) = "Int64"
typeToHaskell (Unnamed (Primitive UInt8)) = "UInt8"
typeToHaskell (Unnamed (Primitive UInt16)) = "UInt16"
typeToHaskell (Unnamed (Primitive UInt32)) = "UInt32"
typeToHaskell (Unnamed (Primitive UInt64)) = "UInt64"
typeToHaskell (Unnamed (Primitive Float16)) = "Float16"
typeToHaskell (Unnamed (Primitive Float32)) = "Float32"
typeToHaskell (Unnamed (Primitive Float64)) = "Float64"
typeToHaskell (Unnamed (Primitive CodepointUnicode)) = "Char"
typeToHaskell (Unnamed (Primitive StringUTF8)) = "String"
typeToHaskell (Unnamed (Primitive (Function sources target))) =
  mconcat ["(", intercalate " -> " $ typeToHaskell <$> fmap snd sources ++ [target], ")"]
typeToHaskell (Unnamed (Primitive (List t))) = mconcat ["[", typeToHaskell t, "]"]
typeToHaskell (Unnamed (Any ident)) = snakeCase ident
typeToHaskell (Named ident []) = pascalCase ident
typeToHaskell (Named ident args) = "(" ++ pascalCase ident ++ " " ++ unwords (fmap typeToHaskell args) ++ ")"

-- substitute type variables with TenkeiValue
typeToHaskell' :: Type -> String
typeToHaskell' (Unnamed (Primitive (Function s t))) = "(" ++ intercalate " -> " ((fmap snd s ++ [t]) >> ["TenkeiValue"]) ++ ")"
typeToHaskell' (Unnamed (Primitive (List t))) = mconcat ["[", typeToHaskell' t, "]"]
typeToHaskell' (Unnamed (Any _)) = "TenkeiValue"
typeToHaskell' (Named ident []) = pascalCase ident
typeToHaskell' (Named ident args) = "(" ++ pascalCase ident ++ " " ++ unwords (fmap typeToHaskell' args) ++ ")"
typeToHaskell' x = typeToHaskell x

generateFunSignature :: FunDef -> (Type -> String) -> Bool -> String
generateFunSignature f@(FunDef _ sources target) converter addConstraints = constraints ++ intercalate " -> " (fmap converter types)
  where
    types = fmap snd sources ++ [target]
    constraints =
      mif
        (addConstraints && hasTypeVars f)
        ("(" ++ intercalate ", " ((("Tenkei " ++) . snakeCase) <$> nub (types >>= getTypeVars)) ++ ") => ")

mif :: Monoid m => Bool -> m -> m
mif True = id
mif False = const mempty

externalSignature :: String
externalSignature = "Ptr Word8 -> CSize -> Ptr (Ptr Word8) -> Ptr CSize -> IO ()"

funDefToExport :: FunDef -> [String]
funDefToExport f@(FunDef name sources _) =
  (mconcat . snd) <$>
  filter
    fst
    [ (typeVars, [foreignName, "_helper :: ", generateFunSignature f typeToHaskell' False])
    , (typeVars, [foreignName, "_helper = ", functionId name])
    , (True, [foreignName, " :: ", externalSignature])
    , (True, [foreignName, " = ", funImpl])
    , (True, ["foreign export ccall ", foreignName, " :: ", externalSignature])
    , (True, [""])
    ]
  where
    foreignName = foreignFunctionId name
    typeVars = hasTypeVars f
    argList = fmap (("arg" ++) . show) [1 .. length sources]
    lambdaArgs s = fmap (("x" ++) . show) [0..length s - 1]
    offerName =
      if typeVars
        then foreignName ++ "_helper"
        else functionId name
    funImpl =
      mconcat
        [ "offerCBOR (\\(CBOR_Array ["
        , intercalate ", " argList
        , "]) -> serialize $ "
        , offerName
        , " "
        , unwords $
          zipWith
            (\arg (_, argType) ->
               case argType of
                 (Unnamed (Primitive (Function s _))) -> "(TenkeiValue . (fromFunPointer (deserialize " ++ arg ++ ")) . (\\" ++ unwords (lambdaArgs s) ++ " -> CBOR_Array [" ++ intercalate "," (("getValue " ++) <$> lambdaArgs s) ++ "]))"
                 _ -> "(deserialize " ++ arg ++ ")")
            argList
            sources
        , ")"
        ]

usePointer :: Type -> Bool
usePointer _ = False
--usePointer = hasTypeVar

funDefToImport :: FunDef -> [String]
funDefToImport f@(FunDef name sources target) =
  fmap
    mconcat
    [ ["foreign import ccall \"", foreignName, "\" foreign_", foreignFunctionId name, " :: ", externalSignature]
    , [functionId name, " :: ", generateFunSignature f typeToHaskell True]
    , [functionId name, " ", unwords argList, " = ", funImpl argList]
    , [""]
    ]
  where
    foreignName = foreignFunctionId name
    argList = fmap (("arg" ++) . show) [1 .. length sources]
    cborImpl args =
      mconcat
        ["deserialize $ callCBOR foreign_", foreignName, " tenkei_free $ CBOR_Array [", intercalate ", " $ fmap ("serialize " ++) args, "]"]
    funImpl args =
      intercalate "\n  " $
      join
        [ return "unsafePerformIO $ do"
        , zipWith convArg (fmap snd sources) argList
        , return $
          (if usePointer target
             then "fromPointer $ "
             else "return $ ") ++
          cborImpl (fmap (++ "'") args)
        ]
    convArg arg argName =
      mconcat $
      case arg of
        (Unnamed (Primitive (Function xs _))) ->
          [ argName
          , "' <- toFunPointer (serialize . (\\["
          , intercalate "," $ fmap (("x" ++) . show) [1 .. length xs]
          , "] -> "
          , argName
          , " "
          , unwords $ fmap (\x -> "(deserialize $ getValue x" ++ show x ++ ")") [1 .. length xs]
          , ") . deserialize)"
          ] --, " tenkei_free"]
        _ ->
          if usePointer arg
            then [argName, "' <- toPointer ", argName]
            else ["let ", argName, "' = ", argName]

typeDefToText :: NamedTypeDef -> [String]
typeDefToText (NamedTypeDef name args (SumParts parts)) =
  [printf "data %s = %s" (typeId name) $ intercalate " | " $ fmap (\(n, t) -> typeId n ++ " " ++ typeToHaskell t) parts]
typeDefToText (NamedTypeDef name args (ProdParts parts)) =
  [ printf "data %s = %s { %s }" (typeId name) (typeId name) $
    intercalate ", " $ fmap (\(n, t) -> functionId n ++ " :: " ++ typeToHaskell t) parts
  ]
typeDefToText (NamedTypeDef name args Opaque) = [printf "data %s = Opaque%s Integer" (typeId name) (typeId name)]
