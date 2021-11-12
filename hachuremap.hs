import Control.Monad (liftM)
import Control.Parallel
import Data.Binary.Get (runGet)
import Graphics.Rendering.Cairo hiding (Matrix)
import Numeric.LinearAlgebra
import System.Random (mkStdGen, randoms)
import qualified Data.ByteString.Lazy as BS
import Gradient
import BinaryExtras
import RandomExtras
import Types
import Unsafe.Coerce
import GHC.Float
import Data.Binary.Get
import Data.Int
import Data.Word

opts = morrowind

main = do
  contents <- BS.readFile $ inputFile opts
  let arr0 = runGet (getManyF (rawFunction opts) $ imageWidth opts*imageHeight opts) contents
  let landscape = (imageHeight opts><imageWidth opts) $ arr0
--  landscape <- liftM ((imageHeight opts><imageWidth opts) . runGet (getAll $ imageWidth opts*imageHeight opts)) $ BS.readFile $ inputFile opts
  let jitter = randoms $ mkStdGen 53
  let [miny, maxy, minx, maxx] = map fromIntegral [1, imageHeight opts-2, 1, imageWidth opts-2]
  let (numpointsy, numpointsx) = (floor $ (maxy-miny)/(gridWidth opts), floor $ (maxx-minx)/(gridWidth opts))
  let regulargrid = [(y,x) | y <- [0..numpointsy-1], x<-[0..numpointsx-1]]
  let jitteredgrid = zipWith (\(y,x) (jy,jx) -> 
        ( miny + (gridWidth opts)* (fromIntegral y + jy)
        , minx + (gridWidth opts) * (fromIntegral x + jx)
        )) regulargrid jitter
  let ascents = map (gradientStepper Ascent (ascentDelta opts) landscape) jitteredgrid
  s <- createImageSurface FormatARGB32 
    (floor $ multiplier opts * (fromIntegral $ imageWidth opts)) 
    (floor $ multiplier opts * (fromIntegral $ imageHeight opts))
  renderWith s $ do
    setSourceRGBA 1 1 1 0.0
    paint
  mapM_ (drawLine (lineWidth opts) (cellWidth opts) (multiplier opts) s) ascents
  surfaceWriteToPNG s $ outputFile opts

drawLine line cell mult s lista = do
  renderWith s $ do
    setLineWidth line
    setLineCap LineCapRound
    go lista
  where
  go ((x1,y1,z1):loput@((x2,y2,z2):_)) = do
    let c = 1.0 - ((atan2 (abs(z1-z2)) cell)/(pi/2)) in setSourceRGB c c c
    moveTo (x1*mult) (y1*mult)
    lineTo (x2*mult) (y2*mult)
    stroke
    go loput
  go _ = return ()

-- Example usage

morrowind = Options
  { inputFile = "tesannwyn.raw"
  , imageWidth = 2688
  , imageHeight = 2816
  , gridWidth = 2.5
  , multiplier = 1.0
  , ascentDelta = 1.0
  , outputFile = "vv.png"
  , lineWidth = 0.35
  , cellWidth = 8.0
  , rawFunction = liftM ((fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) getWord16le
  }

oblivion = Options
  { inputFile = "oblivion.raw"
  , imageWidth = 4288
  , imageHeight = 4128
  , gridWidth = 5
  , multiplier = 1.0
  , ascentDelta = 2.0
  , outputFile = "obl.png"
  , lineWidth = 0.5
  , cellWidth = 8.0
  , rawFunction = liftM ((fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) getWord16le
  }

seworld = Options
  { inputFile = "seworld.raw"
  , imageWidth = 2048
  , imageHeight = 2048
  , gridWidth = 2.5
  , multiplier = 1.0
  , ascentDelta = 1.0
  , outputFile = "se.png"
  , lineWidth = 0.3
  , cellWidth = 8.0
  , rawFunction = liftM ((fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) getWord16le
  }

mokki = Options
  { inputFile = "2mdem.flt"
  , imageWidth = 1857
  , imageHeight = 2136
  , gridWidth = 12.5
  , multiplier = 1.5
  , ascentDelta = 2.0
  , outputFile = "mok.png"
  , lineWidth = 0.5
  , cellWidth = 0.125
  , rawFunction = liftM (float2Double . unsafeCoerce) getWord32le
  }

turku = Options
  { inputFile = "pohja"
  , imageWidth = 513
  , imageHeight = 513
  , gridWidth = 1.25
  , multiplier = 2
  , ascentDelta = 1.0
  , outputFile = "turku.png"
  , lineWidth = 0.5
  , cellWidth = 8
  , rawFunction = liftM ((fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) getWord16le
  }

