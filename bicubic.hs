import Control.Monad
import Control.Parallel
import Data.Binary.Get
import GHC.Float
import Graphics.Rendering.Cairo hiding (Matrix)
import Numeric.LinearAlgebra
import System.Random
import Unsafe.Coerce
import qualified Data.ByteString.Lazy as BS
import Bicubic
import BinaryExtras
import RandomExtras
import Types

opts = mokki

main = do
	contents <- BS.readFile $ inputFile opts
	let arr0 = map (rawFunction opts) $ runGet (getMany $ imageWidth opts*imageHeight opts) contents
	let landscape = (imageHeight opts><imageWidth opts) $ arr0
	let jitter = randoms $ mkStdGen 53
	let [miny, maxy, minx, maxx] = map fromIntegral [1, imageHeight opts-2, 1, imageWidth opts-2]
	let (numpointsy, numpointsx) = (floor $ (maxy-miny)/(gridWidth opts), floor $ (maxx-minx)/(gridWidth opts))
	let startinggrid = [(y,x) | y <- [0..numpointsy-1], x<-[0..numpointsx-1]]
	let startingpoints = zipWith (\(y,x) (jy,jx) -> 
		( miny + (gridWidth opts)* (fromIntegral y + jy)
		, minx + (gridWidth opts) * (fromIntegral x + jx)
		)) startinggrid jitter
	let ascents = map (gradientAscent (ascentDelta opts) landscape) startingpoints
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

morrowind = Options
	{ inputFile = "tesannwyn.raw"
	, imageWidth = 2688
	, imageHeight = 2816
	, gridWidth = 2.5
	, multiplier = 1.0
	, ascentDelta = 1.0
	, outputFile = "vv.png"
	, lineWidth = 0.5
	, cellWidth = 8.0
	, rawFunction = tesToDouble
	}

mokki = Options
	{ inputFile = "2mdem.flt"
	, imageWidth = 1857
	, imageHeight = 2136
	, gridWidth = 2.5
	, multiplier = 1.5
	, ascentDelta = 2.0
	, outputFile = "mok.png"
	, lineWidth = 0.5
	, cellWidth = 0.125
	, rawFunction = rawfloatToDouble
	}

hajoita :: Int -> [a] -> [a]
hajoita n lasku = (foldr1 `par` list) `pseq` concat list
	where
	size = floor $ fromIntegral (length lasku) / fromIntegral n
	list = chunk (n-1) lasku
	chunk 0 l = [l]
	chunk n l = let (a,b) = splitAt size lasku in a : chunk (n-1) b

