module SneakyBeaky.Lifted where

import qualified Control.Monad.Random as R
import Control.Monad.IO.Class(MonadIO,liftIO)

evalRandIO :: MonadIO m => R.Rand R.StdGen a -> m a
evalRandIO = liftIO . R.evalRandIO

