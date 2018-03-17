module Main where

import Types
import Haskell.HaskellTypes

--import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.Either

import System.Environment
import System.IO.Error
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  case args of
    [source,target] -> do
      contents <- fmap (first (\_ -> "Error while reading " ++ source)) $ tryIOError $ readFile source
      parsed <- return $ contents >>= generateHS
      result <- either (return . Left) id $ fmap (fmap (first (\_ -> "Error while writing " ++ target)) . tryIOError . writeFile target) parsed

      either putStrLn (const $ putStrLn "Success!") result

    _ -> putStrLn "The syntax for this command is tenkei [source] [target]"


generateHS text | (Just file) <- decodeType text = Right $ createHaskellFile file
                | otherwise = Left "Error while parsing the souce file"
