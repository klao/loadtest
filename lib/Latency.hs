{-# LANGUAGE RecordWildCards #-}

module Latency where

import Control.Monad.IO.Class
import Data.Int
import Data.IORef
import Text.Printf

import Clock

data Lat = Lat { _count :: {-# UNPACK #-} !Int
               , _sum :: {-# UNPACK #-} !Int64
               , _min :: {-# UNPACK #-} !Int64
               , _max :: {-# UNPACK #-} !Int64
               , _start :: {-# UNPACK #-} !Int64
               }

type LatR = IORef Lat

new :: MonadIO m => m LatR
new = liftIO . newIORef $ Lat 0 0 maxBound 0 (-1)

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
             Lat (_count + 1) (_sum + diff) (min diff _min) (max diff _max) (-1)
           )

print :: MonadIO m => LatR -> m ()
print r = liftIO $ readIORef r >>= p
  where
    p Lat{..} = printf "mean: %.2f,  min: %.2f,  max: %.2f\n" meanms minms maxms
      where
        nsInms = 1000000.0 :: Double
        meanms = fromIntegral _sum / fromIntegral _count / nsInms
        minms = fromIntegral _min / nsInms
        maxms = fromIntegral _max / nsInms
