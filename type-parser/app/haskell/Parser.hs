{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Haskell.Parser where

import Data.Char
import Data.Functor.Identity
import Data.List

import Text.Parsec
import Text.ParserCombinators.Parsec

import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as TokenGen

import Types hiding (upper)

hs = TokenGen.makeTokenParser haskellDef
lexeme = TokenGen.lexeme hs
braces = TokenGen.braces hs
whiteSpace = TokenGen.whiteSpace hs

identifier :: Stream s Identity Char => Parsec s u Identifier
identifier = undefined

-- TODO: Recognize numbers
pascalCaseIdentifier :: Stream s Identity Char => Parsec s u Identifier
pascalCaseIdentifier = many $ do
  c <- upper
  rest <- many lower
  return (c : rest)

camelCaseIdentifier :: Stream s Identity Char => Parsec s u Identifier
camelCaseIdentifier = do
  initial <- many1 lower
  rest <- pascalCaseIdentifier
  return (initial : rest)

primitiveType :: Stream s Identity Char => Parsec s u PrimitiveType
primitiveType = (string "Int32" >> return Int32)

typeParser :: Stream s Identity Char => Parsec s u Type
typeParser = (fmap Primitive $ Text.Parsec.try primitiveType) <|> fmap Composite  pascalCaseIdentifier

function :: Parsec String u FunDef
function = do
  name <- lexeme camelCaseIdentifier
  lexeme (string "::")
  source <- lexeme typeParser
  lexeme (string "->")
  target <- lexeme typeParser
  return $ FunDef name source target

typePartsSum :: Parsec String u TypeParts
typePartsSum = fmap SumParts $ sepBy1 constructorParser $ lexeme $ string "|"
  where constructorParser = do
          identifier <- lexeme pascalCaseIdentifier
          typeName <- lexeme typeParser
          return (identifier, typeName)

typePartsProduct :: Parsec String u TypeParts
typePartsProduct = do
  lexeme pascalCaseIdentifier
  braces (fmap ProdParts $ sepBy1 constructorParser $ lexeme $ string ",")
  where constructorParser = do
          identifier <- lexeme camelCaseIdentifier
          lexeme (string "::")
          typeName <- lexeme typeParser
          return (identifier,typeName)

typeDef :: Parsec String u TypeDef
typeDef = do
  lexeme (string "data")
  name <- lexeme pascalCaseIdentifier
  lexeme (string "=")
  parts <- Text.Parsec.try typePartsProduct <|> typePartsSum
  return $ TypeDef name parts

lineEnd :: Stream s Identity Char => Parsec s u ()
lineEnd = eof <|> (endOfLine >> return ())

indentedLine :: Stream s Identity Char => Parsec s u String
indentedLine = do
  c <- space
  cs <- manyTill anyChar lineEnd
  return (c : cs)

unindentedLine :: Stream s Identity Char => Parsec s u String
unindentedLine = do
  c <- letter <|> oneOf "{}-"
  cs <- manyTill anyChar lineEnd
  return (c : cs)

codeBlock :: Parsec String u String
codeBlock = do
  l <- unindentedLine
  ls <- many indentedLine
  return (intercalate "\n" (l : ls))

