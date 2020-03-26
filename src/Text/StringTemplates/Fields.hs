{-# LANGUAGE CPP, MultiParamTypeClasses, TypeFamilies #-}
-- | Module for easy creating template params
--
-- Example usage:
--
-- @
-- \-- applicable to renderTemplate, renderTemplateI functions
-- fields :: Fields Identity ()
-- fields = do
--   value \"foo\" \"bar\"
--   valueM \"foo2\" $ return \"bar2\"
--   object \"foo3\" $ do
--            value \"foo31\" \"bar31\"
--            value \"foo32\" \"bar32\"
--   objects \"foo4\" [ do
--                    value \"foo411\" \"bar411\"
--                    value \"foo412\" \"bar412\"
--                  , do
--                    value \"foo421\" \"bar421\"
--                    value \"foo422\" \"bar422\"
--                  ]
--
-- \-- applicable to renderTemplateMain functions
-- params :: [(String, SElem String)]
-- params = runIdentity $ runFields fields
-- @
module Text.StringTemplates.Fields ( Fields(..)
                                   , runFields
                                   , value
                                   , valueM
                                   , object
                                   , objects
                                   ) where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..), ComposeSt, defaultLiftBaseWith, defaultRestoreM, defaultLiftWith, defaultRestoreT)
import Data.Int
import Text.StringTemplate.Base hiding (ToSElem, toSElem, render)
import Text.StringTemplate.Classes hiding (ToSElem, toSElem)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as M
import qualified Text.StringTemplate.Classes as HST

import Text.StringTemplates.TemplatesLoader ()

type InnerFields = StateT [(String, SElem String)]

-- | Simple monad transformer that collects info about template params
newtype Fields m a = Fields { unFields :: InnerFields m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadTrans, MonadThrow, MonadCatch, MonadMask)

instance MonadBaseControl b m => MonadBaseControl b (Fields m) where
#if MIN_VERSION_monad_control(1,0,0)
  type StM (Fields m) a = ComposeSt Fields m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
#else
  newtype StM (Fields m) a = StM { unStM :: ComposeSt Fields m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
#endif
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl Fields where
#if MIN_VERSION_monad_control(1,0,0)
  type StT Fields m = StT InnerFields m
  liftWith = defaultLiftWith Fields unFields
  restoreT = defaultRestoreT Fields
#else
  newtype StT Fields m = StT { unStT :: StT InnerFields m }
  liftWith = defaultLiftWith Fields unFields StT
  restoreT = defaultRestoreT Fields unStT
#endif
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

-- | get all collected template params
runFields :: Monad m => Fields m () -> m [(String, SElem String)]
runFields (Fields f) = execStateT f []

-- | create a new named template parameter
value :: (Monad m, ToSElem a) => String -> a -> Fields m ()
value name val = Fields $ modify ((name, toSElem val) :)

-- | create a new named template parameter (monad version)
valueM :: (Monad m, ToSElem a) => String -> m a -> Fields m ()
valueM name mval = lift mval >>= value name

-- | collect all params under a new namespace
object :: Monad m => String -> Fields m () -> Fields m ()
object name obj = Fields $ do
  val <- M.fromList `liftM` lift (runFields obj)
  modify ((name, toSElem val) :)

-- | collect all params under a new list namespace
objects :: Monad m => String -> [Fields m ()] -> Fields m ()
objects name objs = Fields $ do
  vals <- mapM (liftM M.fromList . lift . runFields) objs
  modify ((name, toSElem vals) :)

-- | Important Util. We overide default serialisation to support serialisation of bytestrings .
-- | We use ByteString with UTF all the time but default is Latin-1 and we get strange chars
-- | after rendering. !This will not always work with advanced structures.! So always convert to String.

class ToSElem a where
  toSElem :: (Stringable b) => a -> SElem b

instance {-# OVERLAPPING #-} (HST.ToSElem a) => ToSElem a where
  toSElem = HST.toSElem

instance {-# OVERLAPPING #-}ToSElem Int16 where
  toSElem = toSElem . toInteger

instance {-# OVERLAPPING #-}ToSElem Int32 where
  toSElem = toSElem . toInteger

instance {-# OVERLAPPING #-}ToSElem Int64 where
  toSElem = toSElem . toInteger

instance {-# OVERLAPPING #-}ToSElem BS.ByteString where
  toSElem = HST.toSElem . BS.toString

instance {-# OVERLAPPING #-}ToSElem (Maybe BS.ByteString) where
  toSElem = HST.toSElem . fmap BS.toString

instance {-# OVERLAPPING #-}ToSElem [BS.ByteString] where
  toSElem = toSElem . fmap BS.toString

instance {-# OVERLAPPING #-}ToSElem String where
  toSElem = HST.toSElem

instance {-# OVERLAPPING #-} (HST.ToSElem a) => ToSElem [a] where
  toSElem = LI . map HST.toSElem

instance {-# OVERLAPPING #-} (HST.ToSElem a) => ToSElem (M.Map String a) where
  toSElem = SM . M.map HST.toSElem
