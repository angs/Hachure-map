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

