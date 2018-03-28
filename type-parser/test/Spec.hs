import Test.Hspec

import Parsers.Haskell
import Parsers.Rust
import Types

parsedTestLib :: Maybe DefFile
parsedTestLib =
  Just $
  DefFile
    ["test", "module"]
    [ FunDef ["triple"] (Primitive Int32) (Primitive Int32)
    , FunDef ["quadruple"] (Primitive Int32) (Primitive Int32)
    , FunDef ["quintuple"] (Primitive Int32) (Primitive Int32)
    ]
    [ TypeDef ["test", "type"] $ ProdParts [(["test"], Primitive Int32), (["other", "test"], Primitive Int32)]
    , TypeDef ["test", "type2"] $ SumParts [(["test"], Primitive Int32), (["other", "test"], Primitive Int32)]
    ]

main :: IO ()
main =
  hspec $ do
    describe "Haskell parser" $ do
      it "parses simple haskell files" $ parseHaskell "module Test where\n" `shouldBe` (Just $ DefFile ["test"] [] [])
      it "parses more complicated haskell files" $ fmap parseHaskell (readFile "test/testLib.hs") >>= (`shouldBe` parsedTestLib)
    describe "Rust parser" $ do
      it "parses more complicated Rust files" $ fmap parseRust (readFile "test/testLib.rs") >>= (`shouldBe` parsedTestLib)
