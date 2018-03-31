{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

module Parsers.Haskell
  ( parseHaskell
  ) where

import Control.Monad
import Data.Either
import Data.Either.Combinators
import Data.List
import Data.Maybe
import Text.Printf

import Parsers.GeneralParsers

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

parseHaskell :: String -> Maybe DefFile
parseHaskell s = do
  blocks <- rightToMaybe $ parse (many codeBlock) "Haskell code" s
  (moduleName, exports) <- listToMaybe $ rights $ fmap (parse moduleDef "Type definitions") blocks
  let inExports = inExports' exports
  let typeDefs = filter (inExports . typeName) $ rights $ fmap (parse typeDef "Type definitions") blocks
  let funDefs = filter (inExports . funName) $ rights $ fmap (parse function "Function definitions") blocks
  return $ DefFile moduleName funDefs typeDefs
  where
    inExports' :: Maybe [Identifier] -> Identifier -> Bool
    inExports' (Just l) i = i `elem` l
    inExports' Nothing _ = True

moduleDef :: Parser (Identifier, Maybe [Identifier])
moduleDef = do
  _ <- lexeme $ string "module"
  moduleName <- lexeme pascalCaseIdentifier
  exports <- optional $ parens $ sepBy1 camelCaseIdentifier $ symbol ","
  _ <- lexeme $ string "where"
  eof
  return (moduleName, exports)

primitiveType :: Parser PrimitiveType
primitiveType =
  (string "Int32" >> return Int32) <|> (string "Int64" >> return Int64) <|> (string "Char" >> return CodepointUnicode) <|>
  (List <$> brackets typeParser)

typeParser :: Parser Type
typeParser = ((Unnamed . Primitive) <$> try primitiveType) <|> fmap Named pascalCaseIdentifier

augmentParameters :: [Type] -> [Variable]
augmentParameters [type_] = [(["param"], type_)]
augmentParameters types = fmap (\(i, type_) -> ([printf "param%d" i], type_)) $ zip ([0..] :: [Integer]) types

function :: Parser FunDef
function = do
  name <- lexeme camelCaseIdentifier
  _ <- symbol "::"
  types <- sepBy1 (lexeme typeParser) $ symbol "->"
  return $ FunDef name (augmentParameters $ init types) $ last types

typePartsSum :: Parser NamedType
typePartsSum = fmap SumParts $ sepBy1 constructorParser $ lexeme $ string "|"
  where
    constructorParser = do
      identifier <- lexeme pascalCaseIdentifier
      typeName <- lexeme typeParser
      return (identifier, typeName)

typePartsProduct :: Parser NamedType
typePartsProduct = do
  _ <- lexeme pascalCaseIdentifier
  braces (fmap ProdParts $ sepBy1 constructorParser $ lexeme $ string ",")
  where
    constructorParser = do
      identifier <- lexeme camelCaseIdentifier
      _ <- lexeme (string "::")
      typeName <- lexeme typeParser
      return (identifier, typeName)

typeDef :: Parser NamedTypeDef
typeDef = do
  _ <- lexeme (symbol "data")
  name <- lexeme pascalCaseIdentifier
  _ <- lexeme (symbol "=")
  parts <- try typePartsProduct <|> typePartsSum
  return $ NamedTypeDef name parts

lineEnd :: Parser ()
lineEnd = eof <|> void eol

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
