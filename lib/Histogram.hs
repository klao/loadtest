{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Histogram (
  Histogram(..),
  empty,
  insert,
  print,
  binsUsed,
  ) where

import Data.Int
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Prelude hiding (print)
import Text.Printf

-- TODO(klao): make it not Int64 specific
-- TODO(klao): maybe add an actual min and max?
data Histogram = Hist { _count :: {-# UNPACK #-} !Int
                      , _sum   :: {-# UNPACK #-} !Int64
                      , _bins  :: !(IntMap Int)
                      } deriving (Show)

empty :: Histogram
empty = Hist 0 0 IM.empty

stepBase :: Double
stepBase = log 10 / 10

-- | Returns the bin number the givel value fits in.
valueBin :: Int64 -> Int
valueBin x = floor $ log (fromIntegral x) / stepBase

-- | Lowest value that
binMin :: Int -> Double
binMin b = exp $ fromIntegral b * stepBase

insert :: Int64 -> Histogram -> Histogram
insert x Hist{..} = Hist (_count+1) (_sum+x) (IM.alter addOne (valueBin x) _bins)
  where
    addOne Nothing  = Just 1
    addOne (Just c) = Just (c+1)

binsUsed :: Histogram -> Int
binsUsed Hist{..} = IM.size _bins

-- | Returns the bin number which contains the given percentile
-- together with the population and position withing that bin.
percentileBin :: Histogram -> Double -> (Int, Int, Int)
percentileBin Hist{..} pct = go position $ IM.toAscList _bins
  where
    position = round $ fromIntegral _count * pct / 100
    go !_ [] = error "percentileBin run out"
    go k ((bin, cbin) : rest) | k <= cbin = (bin, cbin, k)
                              | otherwise = go (k-cbin) rest

-- | Interpolate the percentile.
--
-- TODO(klao): this is probably statistically bogus. Figure out what
-- the formula would be for the tails of a normal and use that
-- instead.
percentileLin :: Histogram -> Double -> Double
percentileLin hist pct = (low * (n - k) + high * k) / n
  where
    (b, fromIntegral -> n, fromIntegral -> k) = percentileBin hist pct
    low = binMin b
    high = binMin (b+1)

print :: (Double -> Double) -> Histogram -> IO ()
print u hist@(Hist{..}) =
  do printf "count: %d,  mean: %.3f\n" _count mean
     printf "  min: %.3f, 1%%: %.3f, 5%%: %.3f,   95%%: %.3f, 99%%: %.3f, max: %.3f\n"
       p0 p1 p5 p95 p99 p100
     mapM_ printBin $ IM.toAscList _bins
  where
    mean = u $ fromIntegral _sum / fromIntegral _count
    [p1, p5, p95, p99] = map (u . percentileLin hist) [1, 5, 95, 99]
    -- TODO(klao): these should be a helper function:
    p0 = u . binMin . fst . IM.findMin $ _bins
    p100 = u . binMin . (+1) . fst . IM.findMax $ _bins
    binScale :: Double
    binScale = 40.0 / (fromIntegral $ maximum $ IM.elems _bins)
    printBin (b, k) = printf "    %8.3f  .. %8.3f : %s\n" low high bar
      where
        low = u $ binMin b
        high = u $ binMin (b+1)
        barLength = round $ fromIntegral k * binScale
        bar = replicate barLength '#'
