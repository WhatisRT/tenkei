module Parsers.GeneralParsers where

import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char

import Types

type Parser = Parsec () String

allowedCharacterLower :: Parser Char
allowedCharacterLower = lowerChar <|> digitChar

allowedCharacterUpper :: Parser Char
allowedCharacterUpper = upperChar <|> digitChar

lowerWord :: Parser String
lowerWord = some allowedCharacterLower

upperWord :: Parser String
upperWord = do
  c <- allowedCharacterUpper
  cs <- many allowedCharacterLower
  return $ toLower c : cs

allCapsWord :: Parser String
allCapsWord = fmap toLower <$> many allowedCharacterUpper

pascalCaseIdentifier :: Parser Identifier
pascalCaseIdentifier = many upperWord

camelCaseIdentifier :: Parser Identifier
camelCaseIdentifier = do
  initial <- lowerWord
  rest <- many upperWord
  return (initial : rest)

snakeCaseIdentifier :: Parser Identifier
snakeCaseIdentifier = sepBy1 lowerWord $ char '_'
