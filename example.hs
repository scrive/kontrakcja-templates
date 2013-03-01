import Text.StringTemplates.Fields
import Text.StringTemplates.TemplatesLoader
import Text.StringTemplates.Templates

import System.Directory
import Control.Monad.Identity
import Text.StringTemplate.Classes

-- applicable to renderTemplate, renderTemplateI functions
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

-- applicable to renderTemplateMain functions
params :: [(String, SElem String)]
params = runIdentity $ runFields fields

textTemplates :: String
textTemplates =    "\"X\",\"COLUMN1\",\"COLUMN2\"\n"
                ++ "\"template1\",\"foo equals $foo$\",\"f==$foo$\"\n"
                ++ "\"template2\",\"foo3.foo31=$foo3.foo31$\"\n"

stringTemplates :: String
stringTemplates = "template3=$template1()$ and also $template2()$\n"
                  ++ "###\n"
                  ++ "template4=foo bar baz\n"
                  ++ "foo bar baz\n"

type MyMonad a = TemplatesT Identity a

main :: IO ()
main = do
  createDirectoryIfMissing True "/tmp/templates_test"
  createDirectoryIfMissing True "/tmp/templates_test/text_templates"
  createDirectoryIfMissing True "/tmp/templates_test/string_templates"

  writeFile "/tmp/templates_test/text_templates/test.csv" textTemplates
  writeFile "/tmp/templates_test/string_templates/templates.st" stringTemplates

  templates <- readGlobalTemplates "/tmp/templates_test/text_templates" ["/tmp/templates_test/string_templates/templates.st"] ["COLUMN1", "COLUMN2"]

  let x1 = runIdentity $ runTemplatesT ("COLUMN1", templates) $ renderTemplate "template3" fields
  putStrLn x1

  let x2 = runIdentity $ runTemplatesT ("COLUMN2", templates) $ renderTemplate "template3" fields -- template2 is taken from COLUMN1 because COLUMN2 doesn't define it
  putStrLn x2
      
