{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Plutus.Certification.Cache where

import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import Data.Map qualified as Map

-- | A cache in some monad
--
--  [@val@]: The values in the cache
--  [@meta@]: Metadata associated with cache entries
data Cache val meta m = Cache
  { lookup :: !(val -> m (Maybe meta))
  , register :: !(val -> meta -> m ())
  }

-- | Initialize a 'Cache' in some 'MonadIO', backed by an 'IORef' 'Map.Map'
newCacheMapIO :: (Ord val, MonadIO m) => IO (Cache val key m)
newCacheMapIO = do
  state <- newIORef Map.empty
  pure $ Cache
    { lookup = \v -> liftIO $
        readIORef state <&> Map.lookup v
    , register = \v m -> liftIO $ atomicModifyIORef' state \s ->
        (Map.insert v m s, ())
    }
