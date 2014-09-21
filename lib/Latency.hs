{-# LANGUAGE RecordWildCards #-}

module Latency where

import Control.Monad.IO.Class
import Data.Int
import Data.IORef

import Clock
import qualified Histogram as H

data Lat = Lat { _start :: {-# UNPACK #-} !Int64
               , _hist :: !H.Histogram
               }

type LatR = IORef Lat

new :: MonadIO m => m LatR
new = liftIO . newIORef $ Lat (-1) H.empty

modify :: MonadIO m => IORef a -> (a -> a) -> m ()
modify r f = liftIO . atomicModifyIORef' r $ (\x -> (f x, ()))

start :: MonadIO m => LatR -> m ()
start r = do
  t <- liftIO getTime
  modify r (\l -> if _start l /= -1 then error "start with already started"
                  else  l{ _start = t }
           )

stop :: MonadIO m => LatR -> m ()
stop r = do
  t <- liftIO getTime
  modify r (\Lat{..} ->
             if _start == -1 then error "stop when not started"
             else let diff = t - _start in
             Lat (-1) (H.insert diff _hist)
           )

print :: MonadIO m => LatR -> m ()
print r = liftIO $ readIORef r >>= p
  where
    p Lat{..} = H.print ns2ms _hist
    ns2ms = (/ 1000000)
