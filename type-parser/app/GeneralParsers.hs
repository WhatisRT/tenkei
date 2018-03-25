module GeneralParsers where

import Data.Char

import Text.Megaparsec
import Text.Megaparsec.Char

import Types

type Parser = Parsec () String

allowedCharacterLower :: Parser Char
allowedCharacterLower = lowerChar <|> digitChar

allowedCharacterUpper :: Parser Char
allowedCharacterUpper = upperChar <|> digitChar

pascalCaseIdentifier :: Parser Identifier
pascalCaseIdentifier = some $ do
  c <- allowedCharacterUpper
  rest <- many allowedCharacterLower
  return $ toLower <$> (c : rest)

camelCaseIdentifier :: Parser Identifier
camelCaseIdentifier = do
  initial <- some allowedCharacterLower
  rest <- pascalCaseIdentifier
  return ((toLower <$> initial) : rest)

snakeCaseIdentifier :: Parser Identifier
snakeCaseIdentifier = sepBy1 (some allowedCharacterLower) $ char '_'
