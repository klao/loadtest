{-# LANGUAGE ViewPatterns #-}

module Clock (
  ClockId(..),
  clockGetTime,
  getTime,
  ) where

import Foreign.Safe
import Foreign.C

data C'timespec = C'timespec {
  _c'timespec'tv_sec :: {-# UNPACK #-} !CLong,
  _c'timespec'tv_nsec :: {-# UNPACK #-} !CLong
  } deriving (Eq,Show)

instance Storable C'timespec where
  sizeOf _ = 2 * sizeOf (undefined :: CLong)
  alignment _ = alignment (undefined :: CLong)
  peek (castPtr -> p) = do
    sec <- peekElemOff p 0
    nsec <- peekElemOff p 1
    return $ C'timespec sec nsec
  poke (castPtr -> p) (C'timespec sec nsec) = do
    pokeElemOff p 0 sec
    pokeByteOff p 1 nsec

foreign import ccall unsafe "time.h clock_gettime" c'clock_gettime
  :: CInt -> Ptr C'timespec -> IO CInt

data ClockId
  = CLOCK_REALTIME
  | CLOCK_MONOTONIC
  | CLOCK_PROCESS_CPUTIME_ID
  | CLOCK_THREAD_CPUTIME_ID
  | CLOCK_MONOTONIC_RAW
  | CLOCK_REALTIME_COARSE
  | CLOCK_MONOTONIC_COARSE
  | CLOCK_BOOTTIME
  deriving (Eq, Show, Enum)

getClockId :: ClockId -> CInt
getClockId = fromIntegral . fromEnum

clockGetTimeInternal :: CInt -> IO C'timespec
clockGetTimeInternal clockId = alloca $ \ptimespec -> do
  r <- c'clock_gettime clockId ptimespec
  if r == 0
    then peek ptimespec
    else fail "c'clock_gettime failed"

clockGetTime :: ClockId -> IO Int64
clockGetTime clockId = do
  C'timespec sec nsec0 <- clockGetTimeInternal (getClockId clockId)
  -- A whacky way of handling leap seconds
  let nsec = max 0 $ min 999999999 nsec0
  return $ fromIntegral sec * 1000000000 + fromIntegral nsec

getTime :: IO Int64
getTime = clockGetTime CLOCK_MONOTONIC
