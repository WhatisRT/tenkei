{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Haskell.Parser (parseHaskell) where

import Data.List
import Data.Either.Combinators
import Data.Either
import Data.Maybe
import Data.Char

import GeneralParsers

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parseHaskell :: String -> Maybe DefFile
parseHaskell s = do
  blocks <- rightToMaybe $ parse (many codeBlock) "Haskell code" s
  moduleName <- listToMaybe $ rights $ fmap (parse moduleDef "Type definitions") blocks
  typeDefs <- return $ rights $ fmap (parse typeDef "Type definitions") blocks
  funDefs <- return $ rights $ fmap (parse function "Function definitions") blocks
  return $ DefFile moduleName funDefs typeDefs

moduleDef :: Parser Identifier
moduleDef = do
  _ <- lexeme $ string "module"
  moduleName <- lexeme pascalCaseIdentifier
  _ <- lexeme $ string "where"
  eof
  return moduleName

primitiveType :: Parser PrimitiveType
primitiveType = (string "Int32" >> return Int32)

typeParser :: Parser Type
typeParser = (fmap Primitive $ try primitiveType) <|> fmap Composite pascalCaseIdentifier

function :: Parser FunDef
function = do
  name <- lexeme camelCaseIdentifier
  _ <- lexeme (string "::")
  source <- lexeme typeParser
  _ <- lexeme (string "->")
  target <- lexeme typeParser
  return $ FunDef name source target

typePartsSum :: Parser TypeParts
typePartsSum = fmap SumParts $ sepBy1 constructorParser $ lexeme $ string "|"
  where constructorParser = do
          identifier <- lexeme pascalCaseIdentifier
          typeName <- lexeme typeParser
          return (identifier, typeName)

typePartsProduct :: Parser TypeParts
typePartsProduct = do
  _ <- lexeme pascalCaseIdentifier
  braces (fmap ProdParts $ sepBy1 constructorParser $ lexeme $ string ",")
  where constructorParser = do
          identifier <- lexeme camelCaseIdentifier
          _ <- lexeme (string "::")
          typeName <- lexeme typeParser
          return (identifier,typeName)

typeDef :: Parser TypeDef
typeDef = do
  _ <- lexeme (symbol "data")
  name <- lexeme pascalCaseIdentifier
  _ <- lexeme (symbol "=")
  parts <- try typePartsProduct <|> typePartsSum
  return $ TypeDef name parts

lineEnd :: Parser ()
lineEnd = eof <|> (eol >> return ())

indentedLine :: Parser String
indentedLine = do
  c <- oneOf " \t"
  cs <- manyTill anyChar lineEnd
  return (c : cs)

unindentedLine :: Parser String
unindentedLine = do
  c <- printChar
  cs <- manyTill anyChar (oneOf "\n")
  return (c : cs)

emptyLine :: Parser String
emptyLine = symbol "\n"

codeBlock :: Parser String
codeBlock = do
  l <- unindentedLine <|> emptyLine
  ls <- many (indentedLine <|> emptyLine)
  return (intercalate "\n" (l : ls))

