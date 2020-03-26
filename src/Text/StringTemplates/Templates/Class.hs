module Text.StringTemplates.Templates.Class where

import Control.Monad.Trans

import Text.StringTemplates.TemplatesLoader

-- | Simple reader monad class that provides access to templates
class Monad m => TemplatesMonad m where
  -- | Get templates (for text templates default language name is used).
  getTemplates :: m Templates
  -- | Get templates (for text templates specified language name is used).
  getTextTemplatesByLanguage :: String -> m Templates

-- | Generic, overlapping instance.
instance {-# OVERLAPABLE #-} (
    Monad (t m)
  , MonadTrans t
  , TemplatesMonad m
  ) => TemplatesMonad (t m) where
    getTemplates = lift getTemplates
    getTextTemplatesByLanguage = lift . getTextTemplatesByLanguage
