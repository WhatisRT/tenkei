{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Parsers.Haskell
  ( parseHaskell
  ) where

import Control.Monad
import Data.Maybe

import Parsers.GeneralParsers

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Types

data ProductHelper = ZeroArg | OneArg Type | MultiArg NamedTypeDef

parseHaskell :: String -> Maybe DefFile
parseHaskell s = do
  (HsModule _ (Module moduleName) _ _ definitions) <- case parseModule s of
             (ParseOk x) -> Just x
             _ -> Nothing
  let funDefs = catMaybes $ fmap parseFunctionType definitions
  let typeDefs = join $ catMaybes $ fmap parseTypeDecl definitions
  return $ DefFile (pascalCaseToIdentifier moduleName) funDefs typeDefs

dummyIdentifiers :: [Identifier]
dummyIdentifiers = fmap (("arg" :) . return . show) ([0..] :: [Integer])

dummyIdentifiers2 :: [Identifier]
dummyIdentifiers2 = fmap (("typeclassarg" :) . return . show) ([0..] :: [Integer])

parseFunctionType :: HsDecl -> Maybe FunDef
parseFunctionType (HsTypeSig _ [name] (HsQualType context funType)) = do
  let (s, t) = maybeFunType
  return $ FunDef (camelCaseToIdentifier $ hsNameToString name) (zip dummyIdentifiers2 (context >>= typeClassToFunctions) ++ extract s) t
  where
    parsedFunType = hsTypeToType funType
    maybeFunType =
      case parsedFunType of
        (Unnamed (Primitive (Function a b))) -> (Just a, b)
        _ -> (Nothing, parsedFunType)
    extract (Just l) = l
    extract Nothing = []
    typeClassToFunctions :: HsAsst -> [Type]
    typeClassToFunctions (name, params) =
      case name of
        UnQual (HsIdent "Ord") ->
          [Unnamed $ Primitive $ Function [(["x"], Unnamed $ Any ["a"]), (["y"], Unnamed $ Any ["a"])] $ Unnamed $ Primitive Bool]
        _ -> undefined
parseFunctionType (HsTypeSig _ names _) = error ("How did this happen?\n" ++ show names)
parseFunctionType _ = Nothing

parseTypeDecl :: HsDecl -> Maybe [NamedTypeDef]
parseTypeDecl (HsDataDecl _ _ name params constructors _) =
  Just $ NamedTypeDef convName convParams (fst constrHelper) : snd constrHelper
  where
    convName = convertTypeIdentifier name
    convParams = fmap convertTypeParameters params
    constrHelper = convertToSum constructors
parseTypeDecl _ = Nothing

convertToSum :: [HsConDecl] -> (NamedType, [NamedTypeDef])
convertToSum x = (SumParts $ fmap fst types, catMaybes $ fmap snd types)
  where
    types = fmap hsConDeclToTypes x

hsConDeclToTypes :: HsConDecl -> ((Identifier, Type), Maybe NamedTypeDef)
hsConDeclToTypes (HsConDecl _ name hsType) =
  uncurry productHelpersToTypes (convName, hsProductToProductHelper (fmap unbang hsType) convName)
  where
    convName = convertTypeIdentifier name
    unbang (HsBangedTy x) = x
    unbang (HsUnBangedTy x) = x
hsConDeclToTypes HsRecDecl{} = error "Currently there is no support for record constructors!"

productHelpersToTypes :: Identifier -> ProductHelper -> ((Identifier, Type), Maybe NamedTypeDef)
productHelpersToTypes x ZeroArg = ((x, Unnamed $ Primitive Unit), Nothing)
productHelpersToTypes x (OneArg y) = ((x, y), Nothing)
productHelpersToTypes x (MultiArg def) = ((x, Named (typeName def) []), Just def)

hsProductToProductHelper :: [HsType] -> Identifier -> ProductHelper
hsProductToProductHelper [] _ = ZeroArg
hsProductToProductHelper [x] _ = OneArg $ hsTypeToType x
hsProductToProductHelper x i = MultiArg $ NamedTypeDef (i ++ ["helper"]) [] $ ProdParts $ zip dummyIdentifiers $ fmap hsTypeToType x

hsTypeToType :: HsType -> Type
hsTypeToType (HsTyFun a b) = Unnamed $ Primitive $ Function (zip dummyIdentifiers $ init typeList) $ last typeList
  where
    unwrapFunction a (HsTyFun b c) = a : unwrapFunction b c
    unwrapFunction a b = [a, b]
    typeList = hsTypeToType <$> unwrapFunction a b
hsTypeToType (HsTyTuple types) = Unnamed $ UnnamedProduct (fmap hsTypeToType types)
hsTypeToType (HsTyApp a b) = case a of
             (HsTyCon (Special HsListCon)) -> Unnamed $ Primitive $ List y
             _ -> Named name (y:args)
  where
    Named name args = hsTypeToType a
    y = hsTypeToType b
hsTypeToType (HsTyVar x) = Unnamed $ Any $ convertTypeParameters x
hsTypeToType (HsTyCon (Qual _ _)) = undefined
hsTypeToType (HsTyCon (UnQual x)) =
  case hsNameToString x of
    "Bool" -> Unnamed $ Primitive Bool
    "Int8" -> Unnamed $ Primitive Int8
    "Int16" -> Unnamed $ Primitive Int16
    "Int32" -> Unnamed $ Primitive Int32
    "Int64" -> Unnamed $ Primitive Int64
    "Word8" -> Unnamed $ Primitive UInt8
    "Word16" -> Unnamed $ Primitive UInt16
    "Word32" -> Unnamed $ Primitive UInt32
    "Word64" -> Unnamed $ Primitive UInt64
    "Char" -> Unnamed $ Primitive CodepointUnicode
    _ -> Named (convertTypeIdentifier x) []
hsTypeToType (HsTyCon (Special HsUnitCon)) = undefined
hsTypeToType (HsTyCon (Special HsListCon)) = undefined
hsTypeToType (HsTyCon (Special HsFunCon)) = undefined
hsTypeToType (HsTyCon (Special (HsTupleCon _))) = undefined
hsTypeToType (HsTyCon (Special HsCons)) = undefined

hsNameToString :: HsName -> String
hsNameToString (HsIdent x) = x
hsNameToString (HsSymbol x) = x

convertTypeIdentifier :: HsName -> Identifier
convertTypeIdentifier (HsIdent x) = pascalCaseToIdentifier x
convertTypeIdentifier (HsSymbol x) = pascalCaseToIdentifier x

convertTypeParameters :: HsName -> Identifier
convertTypeParameters (HsIdent x) = [x]
convertTypeParameters (HsSymbol _) = undefined
