module Main (main) where

import LanguageFunctions
import Types

import Generators.C
import Generators.Cpp
import Generators.Haskell
import Generators.Python

import Parsers.Haskell
import Parsers.Rust

import Data.Bifunctor
import Data.List
import Data.Maybe

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
  , ("generate", "Generate sourcecode from a library definition", generate)
  , ("parse", "Parse sourcecode to generate a library definition", parse)
  ]

languages :: [(String, (String -> Maybe DefFile, LanguageGenerators))]
languages =
  [ ("c", (undefined, cLanguageGenerators))
  , ("cpp", (undefined, cppLanguageGenerators))
  , ("haskell", (parseHaskell, haskellLanguageGenerators))
  , ("python", (undefined, pythonLanguageGenerators))
  , ("rust", (parseRust, error "Rust tenkei library generation not yet supported!"))
  ]

fromJustError :: Maybe a -> String -> ErrorIO a
fromJustError (Just x) _ = return x
fromJustError _ x = throwError x

fillRight :: String -> Int -> String
fillRight s i
  | length s < i = fillRight (s ++ " ") i
  | otherwise = s

(!!?) :: [a] -> Int -> Maybe a
(!!?) l n = listToMaybe $ drop n l

selectLanguage :: String -> ErrorIO (String -> Maybe DefFile, LanguageGenerators)
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

scanArg :: String -> [String] -> Maybe String
scanArg s l | (_,b) <- break (== "--" ++ s) l = case b of
                _:v:_ -> Just v
                _ -> Nothing

getCmd :: ErrorIO String
getCmd = do
  args <- liftIO getArgs
  fromJustError (args !!? 0) "No command given"

getLang :: ErrorIO String
getLang = do
  args <- liftIO getArgs
  fromJustError (args !!? 1) "No language specified"

getSource :: ErrorIO String
getSource = do
  args <- liftIO getArgs
  filename <- fromJustError (args !!? 2) "No source file specified"
  customError (readFile filename) (\e -> "Error while reading " ++ filename ++ ":\n" ++ show e)

writeTarget :: String -> ErrorIO ()
writeTarget contents = do
  args <- liftIO getArgs
  maybe (liftIO $ putStr contents) (\x -> customError (writeFile x contents) (\e -> "Error while writing file " ++ x ++ ":\n" ++ show e)) (args !!? 3)

writeFileError :: String -> String -> ErrorIO ()
writeFileError fileName contents =
  customError (writeFile fileName contents) (\e -> "Error while writing file " ++ fileName ++ ":\n" ++ show e)

customArgFile :: String -> ErrorIO (Maybe String)
customArgFile arg = do
  args <- liftIO getArgs
  return $ scanArg arg args

generate :: ErrorIO ()
generate = do
  src <- getSource
  parsed <- fromJustError (decodeType src) "Error while parsing the source file!"
  (_, languageGenerators) <- getLang >>= selectLanguage
  let info =
        [(customArgFile "interface", interfaceGen languageGenerators parsed), (customArgFile "library", libraryGen languageGenerators parsed)] ++
        fmap (f parsed) (others languageGenerators)
  void $ traverse g info
  where
    f parsed (a, b) = (customArgFile a, b parsed)
    g :: (ErrorIO (Maybe String), String) -> ErrorIO ()
    g (a, b) =
      a >>=
      (\x ->
         case x of
           Just y -> writeFileError y b
           Nothing -> return ())

help :: ErrorIO ()
help = liftIO help'

help' :: IO ()
help' =
  putStr $
  intercalate "\n" $
  ["Tool for creating language bindings.", "Usage: tenkei [command] [language] [source] [args]", "", "Commands:"] ++
  fmap (\(c, h, _) -> printf "  %s%s" (fillRight c 10) h) actions ++
  ["", "Arguments:", "--interface: Path of the interface file", "--library: Path of the library file", ""]

help2 :: ErrorIO ()
help2 = do
  cmd <- getCmd
  liftIO (putStr ("Unrecognized command \"" ++ cmd ++ "\"! \n \n")) >> help

parse :: ErrorIO ()
parse = do
  lang <- getLang
  (parser, _) <- selectLanguage lang
  src <- getSource
  parsed <- fromJustError (parser src) "Error while parsing the source file!"
  writeTarget $ generateDefFile parsed
