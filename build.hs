import Data.Bifunctor
import Data.List
import Data.Maybe

import System.Environment
import System.Process
import System.IO.Error

import Control.Monad.Except

type ErrorIO = ExceptT String IO

customError :: IO a -> (IOError -> String) -> ErrorIO a
customError io f = ExceptT $ first f <$> tryIOError io

fromJustError :: Maybe a -> String -> ErrorIO a
fromJustError (Just x) _ = return x
fromJustError _ x = throwError x

(!!?) :: [a] -> Int -> Maybe a
(!!?) l n = listToMaybe $ drop n l

processInDir :: String -> String -> IO String
processInDir dir cmd = readCreateProcess (CreateProcess (ShellCommand cmd) (Just dir) Nothing Inherit Inherit Inherit False False False False False False Nothing Nothing False) ""

languageList :: [String]
languageList = ["c", "haskell"]

main :: IO ()
main = do
  result <-
    runExceptT $ do
      test <- getTestFlag
      if test
        then do
          results <- sequence [build lang1 lang2 >> liftIO (performTest lang1 lang2) | lang1 <- languageList, lang2 <- languageList]
          return $ unlines results
        else do
          lang1 <- getLang1
          lang2 <- getLang2
          build lang1 lang2
  case result of
    (Left s) -> putStrLn (s ++ "\n\n") >> help
    (Right s) -> putStrLn ("\n\n" ++ s)

performTest :: String -> String -> IO String
performTest lang1 lang2 = do
  result <- processInDir "tenkei-build" "env LD_LIBRARY_PATH=. ./test-exe"
  comp <- readFile "tenkei-build/spec"
  let resString = lang1 ++ ", " ++ lang2 ++ ": " ++
        if result == comp
          then "Success!"
          else "Test not successful!"
  putStrLn resString
  return resString

getTestFlag :: ErrorIO Bool
getTestFlag = do
  args <- liftIO getArgs
  return (0 < length (filter (== "--test") args))

getLang1 :: ErrorIO String
getLang1 = do
  args <- liftIO getArgs
  fromJustError (args !!? 0) "This command requires 2 languages!"

getLang2 :: ErrorIO String
getLang2 = do
  args <- liftIO getArgs
  fromJustError (args !!? 1) "This command requires 2 languages!"

build :: String -> String -> ErrorIO String
build lang1 lang2 = do
  _ <- executeProcesses (lang2 ++ "/lib")
  _ <- liftIO $ callCommand ("cp " ++ lang2 ++ "/lib/libtest-library.dylib tenkei-build/")
  _ <- executeProcesses (lang1 ++ "/app")
  _ <- liftIO $ callCommand ("cp " ++ lang1 ++ "/app/test-exe tenkei-build/")
  return "Build successful!"

executeProcesses :: String -> ErrorIO String
executeProcesses dir = liftIO $ do
  cmds <- fmap lines $ readFile $ dir ++ "/build"
  unlines <$> traverse (processInDir dir) cmds

help :: IO ()
help =
  putStr $
  intercalate
    "\n"
    ["Build tool for tenkei. Build a library in language 2 and an executable in language 1.", "Usage: ./build.hs [language1] [language2]"]
