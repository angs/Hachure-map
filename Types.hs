module Types where

import Data.Binary
import GHC.Float
import Unsafe.Coerce
import Control.Monad (liftM)
import Data.Binary.Get
import Data.Int

newtype TES = TES {tesToDouble :: Double}
newtype RAWFloat = RAWFloat {rawfloatToDouble :: Double}

instance Binary TES where
  get = liftM (TES . (fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) getWord16le
  put = undefined

instance Binary RAWFloat where
  get = liftM (RAWFloat . float2Double . unsafeCoerce) getWord32le
  put = undefined

data Options a = Options
  { inputFile :: String          -- input file, RAW format
  , imageWidth :: Int            -- RAW image width
  , imageHeight :: Int           -- RAW image height
  , gridWidth :: Double          -- granularity of ascent starting locations wrt to input
  , multiplier :: Double         -- resize output by multiplier
  , ascentDelta :: Double        -- size of ascension step
  , outputFile :: String         -- output file, PNG
  , lineWidth :: Double          -- line width, after multiplication
  , cellWidth :: Double          -- affects the darkness calculations, smaller values give darker lines
  , rawFunction :: (Get Double)  -- function by which to interpret input file values
  }

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getManyF :: (Binary a) => (Get a) -> Int -> Get [a]
getManyF f n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- f
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)

data Direction = Ascent | Descent deriving Eq
