module Main (main) where

import Types
import Haskell.HaskellTypes
import Haskell.Parser

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

actions :: [(String, String, [String] -> ErrorIO ())]
actions = [
  ("create", "Create language bindings from a library definition", createCode),
  ("help", "Print this text", help),
  ("parse", "Parse sourcecode to generate a library definition", parse)
          ]

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
    [sourceFile,targetFile] -> do
      contents <- customError (readFile sourceFile) (\e -> "Error while reading " ++ sourceFile ++ ":\n" ++ show e)
      parsed <- parseLibDef contents
      let newFile = createHaskellFile parsed
      customError (writeFile targetFile newFile) (\e -> "Error while writing " ++ targetFile ++ ":\n" ++ show e)

    _ -> liftIO $ putStrLn "The syntax for this command is: tenkei create [source] [target]"

parseLibDef :: String -> ErrorIO DefFile
parseLibDef text | (Just file) <- decodeType text = return file
                 | otherwise = throwError "Error while parsing the source file"

fillRight :: String -> Int -> String
fillRight s i | length s < i = fillRight (s ++ " ") i
              | otherwise = s

help :: [String] -> ErrorIO ()
help _ = liftIO $ putStr $ intercalate "\n" $ [
  "Tool for creating language bindings.",
  "",
  "Commands:"
                                 ] ++ fmap (\(c,h,_) -> printf "  %s%s" (fillRight c 8) h) actions ++ [""]

parse :: [String] -> ErrorIO ()
parse args =
  case args of
    [sourceFile,targetFile] -> do
      contents <- customError (readFile sourceFile) (\e -> "Error while reading " ++ sourceFile ++ ":\n" ++ show e)
      let parsed = fromJust $ parseHaskell contents
      customError (writeDefFile targetFile parsed) (\e -> "Error while writing " ++ targetFile ++ ":\n" ++ show e)

    _ -> liftIO $ putStrLn "The syntax for this command is: tenkei parse [source] [target]"
