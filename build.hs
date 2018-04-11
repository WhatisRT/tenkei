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

main :: IO ()
main = do
  result <- runExceptT $ do
    lang1 <- getLang1
    lang2 <- getLang2

    _ <- executeProcesses (lang2 ++ "/lib")
    _ <- executeProcesses (lang1 ++ "/app")
    _ <- liftIO $ callCommand ("cp " ++ lang2 ++ "/lib/libtest-library.dylib tenkei-build/")
    liftIO $ callCommand ("cp " ++ lang1 ++ "/app/test-exe tenkei-build/")

  case result of
    (Left s) -> putStrLn (s ++ "\n\n") >> help
    (Right _) -> return ()

getLang1 :: ErrorIO String
getLang1 = do
  args <- liftIO getArgs
  fromJustError (args !!? 0) "This command requires 2 languages!"

getLang2 :: ErrorIO String
getLang2 = do
  args <- liftIO getArgs
  fromJustError (args !!? 1) "This command requires 2 languages!"

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
