{-# LANGUAGE StandaloneDeriving #-}
import System.IO.HVFS.Utils
import Data.List
import Control.Monad
import Control.Monad.Identity

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Control.Exception
import System.IO.HVFS.Utils
import Control.Monad
import Data.Either
import Data.Either.Utils
import Data.Functor
import System.Directory
import Data.List
import qualified Data.Map as Map

import Text.StringTemplates.Files
import Text.StringTemplates.TextTemplates
import Text.StringTemplates.Fields
import Text.StringTemplates.TemplatesLoader
import Text.StringTemplates.Templates hiding (getTemplates)
import Text.StringTemplate.Classes (SElem(..), STShow(..))

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "Test order" [ testCase "getTextTemplatesFor" test_getTemplates
                                 , testCase "getTextTemplatesFor" test_getTextTemplatesFor
                                 , testCase "test fields" test_fields
                                 , testCase "readGlobalTemplates" test_readGlobalTemplates
                                 ]
        ]

textTemplates :: String
textTemplates =    "\"X\",\"COLUMN1\",\"COLUMN2\"\n"
                ++ "\"template1\",\"foo equals $foo$\",\"f==$foo$\"\n"
                ++ "\"template2\",\"foo3.foo31=$foo3.foo31$\"\n"

stringTemplates :: String
stringTemplates = "template3=$template1()$ and also $template2()$\n"
                  ++ "###\n"
                  ++ "template4=foo bar baz\n"
                  ++ "foo bar baz\n"

fields :: Monad m => Fields m ()
fields = do
  value "foo" "bar"
  valueM "foo2" $ return "bar2"
  object "foo3" $ do
           value "foo31" "bar31"
           value "foo32" "bar32"
  objects "foo4" [ do
                   value "foo411" "bar411"
                   value "foo412" "bar412"
                 , do
                   value "foo421" "bar421"
                   value "foo422" "bar422"
                 ]

deriving instance Eq (SElem String)
deriving instance Show (SElem String)
deriving instance Show STShow
instance Eq STShow where
    (==) = undefined

setUp :: IO ()
setUp = do
  createDirectoryIfMissing True "/tmp/templates_test"
  createDirectoryIfMissing True "/tmp/templates_test/text_templates"
  createDirectoryIfMissing True "/tmp/templates_test/string_templates"

  writeFile "/tmp/templates_test/text_templates/test.csv" textTemplates
  writeFile "/tmp/templates_test/string_templates/templates.st" stringTemplates

tearDown :: IO ()
tearDown = do
  removeDirectoryRecursive "/tmp/templates_test"

mktest assertion = bracket setUp (const tearDown) (const assertion)

test_getTemplates :: Assertion
test_getTemplates = mktest $ do
  ts <- getTemplates "/tmp/templates_test/string_templates/templates.st"
  assertEqual "getTemplates" ts
                  [ ("template3", "$template1()$ and also $template2()$")
                  , ("template4", "foo bar baz\r\nfoo bar baz")
                  ]

test_getTextTemplatesFor :: Assertion
test_getTextTemplatesFor = mktest $ do
  ts1 <- getTextTemplatesFor "/tmp/templates_test/text_templates" "COLUMN1" ["COLUMN2"]
  assertEqual "getTextTemplatesFor1" ts1
                  [ ("template1", "foo equals $foo$")
                  , ("template2", "foo3.foo31=$foo3.foo31$")
                  ]
  ts2 <- getTextTemplatesFor "/tmp/templates_test/text_templates" "COLUMN2" []
  assertEqual "getTextTemplatesFor2" ts2 [("template1", "f==$foo$")]
  ts3 <- getTextTemplatesFor "/tmp/templates_test/text_templates" "COLUMN2" ["COLUMN1"]
  assertEqual "getTextTemplatesFor3" ts3
                  [ ("template1", "f==$foo$")
                  , ("template2", "foo3.foo31=$foo3.foo31$")
                  ]

test_fields :: Assertion
test_fields = do
  let fs = runIdentity $ runFields fields
  assertEqual "test fields" (Map.fromList fs) $ Map.fromList
    [ ("foo", STR "bar")
    , ("foo2", STR "bar2")
    , ("foo3", SM $ Map.fromList [ ("foo31", STR "bar31")
                                 , ("foo32", STR "bar32")
                                 ])
    , ("foo4", LI [ SM $ Map.fromList [ ("foo411", STR "bar411")
                                      , ("foo412", STR "bar412")
                                      ]
                  , SM $ Map.fromList [ ("foo421", STR "bar421")
                                      , ("foo422", STR "bar422")
                                      ]
                  ])
    ]

test_readGlobalTemplates :: Assertion
test_readGlobalTemplates = mktest $ do
  ts <- readGlobalTemplates "/tmp/templates_test/text_templates" ["/tmp/templates_test/string_templates/templates.st"] ["COLUMN1", "COLUMN2"]
  let render col name = runIdentity $ runTemplatesT ("COLUMN" ++ show col, ts) $ renderTemplate name fields
  assertEqual "templates1" "foo equals bar" $ render 1 "template1"
  assertEqual "templates2" "foo3.foo31=bar31" $ render 1 "template2"
  assertEqual "templates3" "foo equals bar and also foo3.foo31=bar31" $ render 1 "template3"
  assertEqual "templates4" "foo bar baz\r\nfoo bar baz" $ render 1 "template4"

  assertEqual "templates5" "f==bar" $ render 2 "template1"
  assertEqual "templates6" "foo3.foo31=bar31" $ render 2 "template2"
  assertEqual "templates7" "f==bar and also foo3.foo31=bar31" $ render 2 "template3"
  assertEqual "templates8" "foo bar baz\r\nfoo bar baz" $ render 2 "template4"
