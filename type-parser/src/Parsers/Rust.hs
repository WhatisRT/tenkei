{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Parsers.Rust where --(parseRust) where

import Data.Either
import Data.Either.Combinators

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Parsers.GeneralParsers

import Types

parseRust :: String -> Maybe DefFile
parseRust s = do
  blocks <- rightToMaybe $ parse (many codeBlock) "Rust code" s
  --moduleName <- listToMaybe $ rights $ fmap (parse moduleDef "Type definitions") blocks
  moduleName <- return ["test", "module"]
  typeDefs <- return $ rights $ fmap (parse typeDef "Type definitions") [h ++ b | (MultiLinePub h b) <- blocks]
  funDefs <- return $ rights $ fmap (parse function "Function definitions") [h | (MultiLinePub h _) <- blocks]
  return $ DefFile moduleName funDefs typeDefs

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

{-
moduleDef :: Parser Identifier
moduleDef = do
  lexeme $ string "module"
  moduleName <- lexeme pascalCaseIdentifier
  lexeme $ string "where"
  eof
  return moduleName
 -}
primitiveType :: Parser PrimitiveType
primitiveType = (string "i32" >> return Int32)

typeParser :: Parser Type
typeParser = (Primitive <$> try primitiveType) <|> fmap Composite snakeCaseIdentifier

qualifiedTypeParser1 :: Parser (Identifier, Type)
qualifiedTypeParser1 = do
  identifier <- snakeCaseIdentifier
  _ <- symbol ":"
  typeName <- typeParser
  return (identifier, typeName)

qualifiedTypeParser2 :: Parser (Identifier, Type)
qualifiedTypeParser2 = do
  identifier <- pascalCaseIdentifier
  typeName <- parens typeParser
  return (identifier, typeName)

function :: Parser FunDef
function = do
  _ <- symbol "fn"
  name <- lexeme snakeCaseIdentifier
  (_, source) <- parens qualifiedTypeParser1
  _ <- symbol "->"
  target <- typeParser
  return $ FunDef name source target

typePartsSum :: Parser TypeDef
typePartsSum = do
  _ <- symbol "enum"
  name <- lexeme pascalCaseIdentifier
  parts <- braces $ sepEndBy1 qualifiedTypeParser2 $ symbol ","
  return $ TypeDef name $ SumParts parts

typePartsProduct :: Parser TypeDef
typePartsProduct = do
  _ <- symbol "struct"
  name <- lexeme pascalCaseIdentifier
  parts <- braces $ sepEndBy1 qualifiedTypeParser1 $ symbol ","
  return $ TypeDef name $ ProdParts parts

typeDef :: Parser TypeDef
typeDef = try typePartsProduct <|> typePartsSum

data RustBlock
  = OneLinePub
  | MultiLinePub String
                 String
  | Nested
  deriving (Show)

oneLinePubBlock :: Parser RustBlock
oneLinePubBlock = do
  _ <- lexeme $ symbol "pub"
  _ <- manyTill anyChar $ char ';'
  return OneLinePub

-- currently there is only one level of nesting supported
multiLinePubBlock :: Parser RustBlock
multiLinePubBlock = do
  _ <- lexeme $ symbol "pub"
  description <- manyTill anyChar $ lookAhead $ braces $ many $ noneOf "{}"
  contents <- braces $ many $ noneOf "{}"
  return $ MultiLinePub description ("{" ++ contents ++ "}")

-- currently there is only one level of nesting supported
otherBlock :: Parser RustBlock
otherBlock = do
  _ <- braces $ many $ noneOf "{}"
  return Nested

codeBlock :: Parser RustBlock
codeBlock = try (lexeme oneLinePubBlock) <|> try (lexeme multiLinePubBlock) <|> otherBlock
