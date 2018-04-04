#!/usr/bin/env runhaskell

import System.Environment
import System.Process

libs :: [String]
libs = ["rts", "ghc-prim-0.5.1.1", "base-4.10.1.0", "integer-gmp-1.0.1.0"]

tenkeiLib :: String
tenkeiLib = "HShaskell-test-lib-0.1.0.0-CDH7byAS1KYDcQR3ppvNrO-ghc8.2.2"

main :: IO ()
main = do
  args <- getArgs
  let dir = head args
  let ccArgs = unwords $ fmap (\l -> "-lHS" ++ l ++ " -L" ++ (dir ++ l)) libs
  let command = "cc main.c -lcbor -L. -l" ++ tenkeiLib ++ " -lffi -liconv " ++ ccArgs
  putStrLn command
  callCommand command
