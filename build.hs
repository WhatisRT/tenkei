import Data.List
import Data.Maybe

import System.Environment
import System.Process hiding (runProcess)
import System.IO.Error

import Control.Monad.Except

type ErrorIO = ExceptT String IO

fromJustError :: Maybe a -> String -> ErrorIO a
fromJustError (Just x) _ = return x
fromJustError _ x = throwError x

(!!?) :: [a] -> Int -> Maybe a
(!!?) l n = listToMaybe $ drop n l

fillRight :: String -> Int -> String
fillRight s i
  | length s < i = fillRight (s ++ " ") i
  | otherwise = s

getCreateProcess :: String -> [(String, String)] -> String -> IO CreateProcess
getCreateProcess dir extraEnv cmd = do
  env <- getEnvironment
  return (CreateProcess (ShellCommand cmd) (Just dir) (Just (env ++ extraEnv)) Inherit Inherit Inherit False False False False False False Nothing Nothing False )

runProcess :: String -> [(String, String)] -> String -> IO ()
runProcess dir extraEnv cmd = do
  cp <- getCreateProcess dir extraEnv cmd
  _ <- withCreateProcess cp (\_ _ _ p -> waitForProcess p)
  return ()

getProcessOutput :: String -> [(String, String)] -> String -> IO String
getProcessOutput dir extraEnv cmd = do
  cp <- getCreateProcess dir extraEnv cmd
  readCreateProcess cp ""

languageList :: [String]
languageList = ["c", "cpp", "haskell", "python"]

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
  result <- catchIOError (getProcessOutput "tenkei-build" [("LD_LIBRARY_PATH", ".")] "./test-exe") (\_ -> return "")
  comp <- readFile "tenkei-build/spec"
  let resString =
        fill lang1 ++
        ", " ++
        fill lang2 ++
        ": " ++
        if result == lang1 ++ "\n" ++ lang2 ++ "\n" ++ comp
          then "Success!"
          else "Failure!"
  putStrLn resString
  return resString
  where
    fill s = fillRight s $ maximum $ fmap length languageList

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
  _ <- executeProcesses (lang2 ++ "/lib") []
  _ <- liftIO $ callCommand ("cp " ++ lang2 ++ "/lib/libtest-library.so tenkei-build/")
  _ <- executeProcesses (lang1 ++ "/app") []
  _ <- liftIO $ callCommand ("cp " ++ lang1 ++ "/app/test-exe tenkei-build/")
  return "Build successful!"

executeProcesses :: String -> [(String, String)] -> ErrorIO ()
executeProcesses dir subst = liftIO $ runProcess dir subst "./build"

help :: IO ()
help =
  putStr $
  intercalate
    "\n"
    ["Build tool for tenkei. Build a library in language 2 and an executable in language 1.", "Usage: ./build.hs [language1] [language2]"]
