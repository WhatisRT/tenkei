module LanguageFunctions where

import Types

data LanguageGenerators = LanguageGenerators
  { libraryGen :: DefFile -> String
  , interfaceGen :: DefFile -> String
  , others :: [(String, DefFile -> String)]
  }
