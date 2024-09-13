{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Templates.Templates
-- Maintainer  :  bartek@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- This is main templating module. It provides basic interface for
-- generation templates (RenderTemplate) with 'renderTemplate'
-- function. We also provide types for templates and few functions for
-- loading and testing
--
-- HOW TO USE TEMPLATING SYSTEM
--
-- 1) There is a folder called templates, All templates files (*.st) are there. Each files contains many templates
--    definitions, and there must be line starting with # between each two templates.
--    Template definition has form 'nameOfTemplate=bodyOfTemplate'.
--
-- 2) Template body is just String Template and you should be able to find more
--    info at http://www.haskell.org/haskellwiki/HStringTemplate
--
-- 3) All templates are in a global scope. Watch out for conflicting names.
--    On dev computers they are loaded on every request, so one can change them without stoping server.
--
-- 4) To generate a template in haskell call renderTemplate.
--       First param is a set of all templates. Usually you can get it from 'Context'.
--       Next is a name of template that You wan't to render.
--       Last one is some for of list of params.
--       As a result you get IO String.
--        If template will fail You will get error info inside. But this is only for syntax errors.
--        If You will forget a param there will be info in log, and template set param value to something empty.
--
--
-- FIELDS
--
-- Current policy is to use fields. You can find usage of
-- [(String,String)] as params and also composition of setAttribute
-- functions in a code.  This are old concepts and will be droped at
-- some point.
--
-- How to user fields:
--  - there is one function ('field') that sets one field
--  - fields form a monad so you can use do notation for setting many fields
--  - value of a field can be almoust everything (String, Int, Maybe, List, Map, types that are instances of Data and Typeable etc)
--  - IO wrapped values and fields can be also a values of a field.
--
-- Example
--
-- >      userView tempates user =
-- >        renderTemplate templates "userView" $ do
-- >          userFields
-- >
-- >      userFields user = do
-- >        field "name" $ username user
-- >        field "company" $ usercompany user
-- >        field "documents" $ map (documentFields) getUserDocumentsFromDB
-- >
-- >      documentFields document = do
-- >        field "id" $ documentid document
-- >        field "title" $ documenttitle document
--
--
-- Why we want to use fields
--      - They force reuse. We write documentFields, and reuse it every time we want to pass document info to template.
--      - Fields can be extended. If I want to have extended info about user I use 'userFields' to set basic info and
--        then add advanced fields
--      - No need to first bind from IO, then pass to template
--      - They support advanced structures like lists and maybe's
--
--
-- Some extra info:
--  In templates use maybe. You can use 'if' in template body to check for Nothing
--  Always change ByteString to String. We have a problems with encoding, so please watch for this.
--
-- Please also see example.hs for a running example
-----------------------------------------------------------------------------
module Text.StringTemplates.Templates ( Fields
                                      , runFields
                                      , TemplatesMonad(..)
                                      , renderTemplate
                                      , renderTemplate_
                                      , renderTemplateI
                                      , TemplatesT(..)
                                      , runTemplatesT
                                      , renderHelper
                                      ) where

import Text.StringTemplate.Base hiding (ToSElem, toSElem, render)
import Text.StringTemplates.Templates.Class
import Text.StringTemplates.TemplatesLoader

import Text.StringTemplates.Fields
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Identity hiding (liftCatch)
import Control.Monad.Trans.Control ( MonadBaseControl(..)
                                   , MonadTransControl(..)
                                   , ComposeSt
                                   , defaultLiftBaseWith
                                   , defaultRestoreM
                                   , defaultLiftWith
                                   , defaultRestoreT
                                   )

-- | renders a template by name
renderTemplate :: TemplatesMonad m =>
                 String     -- ^ template name
               -> Fields m () -- ^ template params
               -> m String
renderTemplate name fields = do
  ts <- getTemplates
  renderHelper ts name fields

-- | renders a template by name (params function cannot use side effects)
renderTemplateI :: TemplatesMonad m =>
                  String            -- ^ template name
                -> Fields Identity () -- ^ template params
                -> m String
renderTemplateI name fields = do
  ts <- getTemplates
  return $ renderTemplateMain ts name ([]::[(String, String)]) (setManyAttrib $ runIdentity $ runFields fields)

-- | renders a template by name without any params
renderTemplate_ :: TemplatesMonad m =>
                  String -- ^ template name
                -> m String
renderTemplate_ name = renderTemplate name $ return ()

renderHelper :: Monad m => Templates -> String -> Fields m () -> m String
renderHelper ts name fields = do
  attrs <- runFields fields
  return $ renderTemplateMain ts name ([]::[(String, String)]) (setManyAttrib attrs)

type InnerTemplatesT = ReaderT (String, GlobalTemplates)

-- | Simple implementation of TemplatesMonad
newtype TemplatesT m a = TemplatesT { unTT :: InnerTemplatesT m a }
  deriving (Applicative, Functor, Monad, MonadIO, MonadTrans, MonadBase b, MonadThrow, MonadCatch, MonadMask)

runTemplatesT :: (Functor m, Monad m) =>
                (String, GlobalTemplates) -- ^ (default language name, global templates)
              -> TemplatesT m a -> m a
runTemplatesT ts action = runReaderT (unTT action) ts

instance MonadBaseControl b m => MonadBaseControl b (TemplatesT m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (TemplatesT m) a = ComposeSt TemplatesT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (TemplatesT m) a = StM { unStM :: ComposeSt TemplatesT m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
#endif
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl TemplatesT where
#if MIN_VERSION_monad_control(1,0,0)
  type StT TemplatesT m = StT InnerTemplatesT m
  liftWith = defaultLiftWith TemplatesT unTT
  restoreT = defaultRestoreT TemplatesT
#else
  newtype StT TemplatesT m = StT { unStT :: StT InnerTemplatesT m }
  liftWith = defaultLiftWith TemplatesT unTT StT
  restoreT = defaultRestoreT TemplatesT unStT
#endif
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

instance (Applicative m, Monad m) => TemplatesMonad (TemplatesT m) where
  getTemplates = TemplatesT $ do
    (lang, ts) <- ask
    return $ localizedVersion lang ts
  getTextTemplatesByLanguage lang = TemplatesT $ do
    (_, ts) <- ask
    return $ localizedVersion lang ts

instance MonadError e m => MonadError e (TemplatesT m) where
  throwError = lift . throwError
  catchError m h = TemplatesT $ catchError (unTT m) (unTT . h)
