module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Map as Map
import Text.StringTemplate
import Data.Functor ((<&>))
import qualified Control.Exception as Exception
import Control.Exception (SomeException(..))
import Data.List (isPrefixOf)

import Text.StringTemplates.TemplatesLoader (readGlobalTemplates, GlobalTemplates)



main :: IO ()
main = do
  defaultMain $
    testGroup "Templates Loader" $
      [ testCase "fetch value from text templates" $ do
          globalTemplates <- readGlobalTemplates textFixtures templateFixtures "fr"
          let mRendered = Map.lookup "fr" globalTemplates >>= getStringTemplate "_Hello" <&> render
          assertEqual "assert same values" (Just "Bonjour .") mRendered
          assertEqual "available languages" ["fr", "jp"] $ Map.keys globalTemplates

      , testCase "fetch value from templates" $ do
          globalTemplates <- readGlobalTemplates textFixtures templateFixtures "fr"
          let mRendered = Map.lookup "fr" globalTemplates >>= getStringTemplate "foo" <&> render
          assertEqual "assert same values" (Just "hey") mRendered

      , testCase "missing default language should raise an exception" $ do
          eException <- Exception.try $ readGlobalTemplates textFixtures templateFixtures "cz"
          case eException of
            Left (SomeException e) -> do
              assertBool "exception raised" $ "Default language cz is not defined." `isPrefixOf` (show e)
            _ ->
              assertFailure "missing language didn't raise an exception"

      , testCase "fill template with default language values if missing values" $ do
          globalTemplates <- readGlobalTemplates textFixtures templateFixtures "jp"
          let mRendered = Map.lookup "fr" globalTemplates >>= getStringTemplate "_Hello" <&> render
          assertEqual "assert same values" (Just "Bonjour .") mRendered
          assertEqual "available languages" ["fr", "jp"] $ Map.keys globalTemplates
      ]
    where
      templateFixtures :: String
      templateFixtures = "test/fixtures/templates"

      textFixtures :: String
      textFixtures = "test/fixtures/texts"
