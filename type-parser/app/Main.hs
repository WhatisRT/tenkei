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

actions :: [(String, String, [String] -> ErrorIO ())]
actions = [
  ("create", "Create language bindings from a library definition", createCode),
  ("help", "Print this text", help),
  ("parse", "Parse sourcecode to generate a library definition", parse)
          ]

languages :: [(String,(String -> Maybe DefFile, DefFile -> String))]
languages = [
  ("haskell", (parseHaskell, createHaskellFile)),
  ("rust", (parseRust, createRustFile))]

fromJustError :: Maybe a -> String -> ErrorIO a
fromJustError (Just x) _ = return x
fromJustError _ x = throwError x

fillRight :: String -> Int -> String
fillRight s i | length s < i = fillRight (s ++ " ") i
              | otherwise = s

selectLanguage :: String -> ErrorIO (String -> Maybe DefFile, DefFile -> String)
selectLanguage lang | (Just translators) <- lookup lang languages = return translators
                    | otherwise = throwError ("Unsupported language! Supported languages are:\n"
                                              ++ intercalate ", " (fst <$> languages))

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT $ case args of
    (cmd:rest) -> (maybe help (\(_,_,f) -> f) $ find (\(c,_,_) -> cmd == c) actions) rest
    _ -> help args

  case result of
    (Left s) -> putStrLn s
    (Right _) -> return ()

createCode :: [String] -> ErrorIO ()
createCode args =
  case args of
    [language,sourceFile,targetFile] -> do
      (_,codeGen) <- selectLanguage language
      contents <- customError (readFile sourceFile) (\e -> "Error while reading " ++ sourceFile ++ ":\n" ++ show e)
      parsed <- fromJustError (decodeType contents) "Error while parsing the source file!"
      let newFile = codeGen parsed
      customError (writeFile targetFile newFile) (\e -> "Error while writing " ++ targetFile ++ ":\n" ++ show e)

    _ -> liftIO $ putStrLn "The syntax for this command is: tenkei create [language] [source] [target]"

help :: [String] -> ErrorIO ()
help _ = liftIO $ putStr $ intercalate "\n" $ [
  "Tool for creating language bindings.",
  "",
  "Commands:"
  ] ++ fmap (\(c,h,_) -> printf "  %s%s" (fillRight c 8) h) actions ++ [""]

parse :: [String] -> ErrorIO ()
parse args =
  case args of
    [language,sourceFile,targetFile] -> do
      (parser,_) <- selectLanguage language
      contents <- customError (readFile sourceFile) (\e -> "Error while reading " ++ sourceFile ++ ":\n" ++ show e)
      parsed <- fromJustError (parser contents) "Error while parsing the source file!"
      customError (writeDefFile targetFile parsed) (\e -> "Error while writing " ++ targetFile ++ ":\n" ++ show e)

    _ -> liftIO $ putStrLn "The syntax for this command is: tenkei parse [language] [source] [target]"
