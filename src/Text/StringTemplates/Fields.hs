{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverlappingInstances #-}
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

import Control.Applicative
import Control.Monad.Base (MonadBase)
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

-- | Simple monad transformer that collects info about template params
newtype Fields m a = Fields { unFields :: StateT [(String, SElem String)] m a }
  deriving (Applicative, Functor, Monad, MonadBase b, MonadTrans)

instance MonadBaseControl IO m => MonadBaseControl IO (Fields m) where
  newtype StM (Fields m) a = StM { unStM :: ComposeSt Fields m a }
  liftBaseWith = defaultLiftBaseWith StM
  restoreM     = defaultRestoreM unStM
  {-# INLINE liftBaseWith #-}
  {-# INLINE restoreM #-}

instance MonadTransControl Fields where
  newtype StT Fields m = StT { unStT :: StT (StateT [(String, SElem String)]) m }
  liftWith = defaultLiftWith Fields unFields StT
  restoreT = defaultRestoreT Fields unStT
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

instance (HST.ToSElem a) => ToSElem a where
  toSElem = HST.toSElem

instance ToSElem Int16 where
  toSElem = toSElem . toInteger

instance ToSElem Int32 where
  toSElem = toSElem . toInteger

instance ToSElem Int64 where
  toSElem = toSElem . toInteger

instance ToSElem BS.ByteString where
  toSElem = HST.toSElem . BS.toString

instance ToSElem (Maybe BS.ByteString) where
  toSElem = HST.toSElem . fmap BS.toString

instance ToSElem [BS.ByteString] where
  toSElem = toSElem . fmap BS.toString

instance ToSElem String where
  toSElem l = HST.toSElem l

instance (HST.ToSElem a) => ToSElem [a] where
  toSElem l  = LI $ map HST.toSElem l

instance (HST.ToSElem a) => ToSElem (M.Map String a) where
  toSElem m = SM $ M.map HST.toSElem m
