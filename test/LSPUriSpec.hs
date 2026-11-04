module LSPUriSpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Text as T
import LSPUri (filePathToUriText, uriTextToFilePath)
import System.Info (os)
import Test.Hspec

spec :: Spec
spec = describe "uriTextToFilePath" $ do
  -- These synthetic paths are test fixtures and are never accessed on disk.
  it "decodes a standard VS Code file URI" $ do
    let result = uriTextToFilePath (T.pack "file:///D:/example/path%20with%20spaces/model.hs")
    result `shouldSatisfy` maybe False (not . isInfixOf "%20")

  it "uses the native Windows drive path" $ do
    let result = uriTextToFilePath (T.pack "file:///D:/example/path/model.hs")
        expected
          | os == "mingw32" = Just "D:\\example\\path\\model.hs"
          | otherwise = Just "/D:/example/path/model.hs"
    result `shouldBe` expected

  it "round-trips a native path containing spaces" $ do
    let path
          | os == "mingw32" = "D:\\example\\path with spaces\\model.hs"
          | otherwise = "/tmp/example/path with spaces/model.hs"
    uriTextToFilePath (filePathToUriText path) `shouldBe` Just path
