module Main (main) where

import Types

import Generators.Haskell
import Generators.Rust

import Parsers.Haskell
import Parsers.Rust

import Data.Bifunctor
import Data.List

import System.Environment
import System.IO.Error

import Control.Monad.Except

import Text.Printf

type ErrorIO = ExceptT String IO

customError :: IO a -> (IOError -> String) -> ErrorIO a
customError io f = ExceptT $ first f <$> tryIOError io

actions :: [(String, String, ErrorIO ())]
actions =
  [ ("help", "Print this text", help)
  , ("interface", "Generate a tenkei interface from a library definition", generateInterface)
  , ("library", "Generate language bindings from a library definition", generateLib)
  , ("parse", "Parse sourcecode to generate a library definition", parse)
  ]

languages :: [(String, (String -> Maybe DefFile, DefFile -> String, DefFile -> String))]
languages =
  [ ("haskell", (parseHaskell, generateHaskellLib, generateHaskellInterface))
  , ("rust", (parseRust, createRustFile, error "Rust tenkei library generation not yet supported!"))
  ]

fromJustError :: Maybe a -> String -> ErrorIO a
fromJustError (Just x) _ = return x
fromJustError _ x = throwError x

fillRight :: String -> Int -> String
fillRight s i
  | length s < i = fillRight (s ++ " ") i
  | otherwise = s

selectLanguage :: String -> ErrorIO (String -> Maybe DefFile, DefFile -> String, DefFile -> String)
selectLanguage lang
  | (Just translators) <- lookup lang languages = return translators
  | otherwise = throwError ("Unsupported language! Supported languages are: " ++ intercalate ", " (fst <$> languages))

main :: IO ()
main = do
  result <- runExceptT $ do
      cmd <- getCmd
      maybe help2 (\(_, _, f) -> f) $ find (\(c, _, _) -> cmd == c) actions

  case result of
    (Left s) -> putStrLn (s ++ "\n\n") >> help'
    (Right _) -> return ()

getCmd :: ErrorIO String
getCmd = do
  args <- liftIO getArgs
  case args of
    (x:_) -> return x
    _ -> ExceptT $ return $ Left "No command given"

getLang :: ErrorIO String
getLang = do
  args <- liftIO getArgs
  flip fromJustError "No language specified" $
    case args of
      (_:x:_) -> Just x
      _ -> Nothing

getSource :: ErrorIO String
getSource = do
  args <- liftIO getArgs
  join $
    flip fromJustError "No source file specified" $
    case args of
      (_:_:x:_) -> Just $ customError (readFile x) (\e -> "Error while reading " ++ x ++ ":\n" ++ show e)
      _ -> Nothing

writeTarget :: String -> ErrorIO ()
writeTarget contents = do
  args <- liftIO getArgs
  case args of
    (_:_:_:x:_) -> customError (writeFile x contents) (\e -> "Error while writing file " ++ x ++ ":\n" ++ show e)
    _ -> liftIO $ putStr contents

generateLib :: ErrorIO ()
generateLib = do
  (_, codeGen, _) <- getLang >>= selectLanguage
  generateCode codeGen

generateInterface :: ErrorIO ()
generateInterface = do
  (_, _, codeGen) <- getLang >>= selectLanguage
  generateCode codeGen

generateCode :: (DefFile -> String) -> ErrorIO ()
generateCode codeGen = do
  src <- getSource
  parsed <- fromJustError (decodeType src) "Error while parsing the source file!"
  writeTarget $ codeGen parsed

help :: ErrorIO ()
help = liftIO help'

help' :: IO ()
help' = putStr $ intercalate "\n" $
  [ "Tool for creating language bindings."
  , "Usage: tenkei [command] [language] [source] [target]"
  , ""
  , "Commands:"
  ] ++ fmap (\(c,h,_) -> printf "  %s%s" (fillRight c 10) h) actions ++ [""]

help2 :: ErrorIO ()
help2 = do
  cmd <- getCmd
  liftIO (putStr ("Unrecognized command \"" ++ cmd ++ "\"! \n \n")) >> help

parse :: ErrorIO ()
parse = do
  lang <- getLang
  (parser, _, _) <- selectLanguage lang
  src <- getSource
  parsed <- fromJustError (parser src) "Error while parsing the source file!"
  writeTarget $ generateDefFile parsed
