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

main = do
	contents <- BS.readFile "2mdem.flt"
	let width = 1857
	let height = 2136
	let arr0 = map rawfloatToDouble $ runGet (getMany $ height*width) contents
	--let !arr1 = {-# SCC "arr1" #-} map ((fromIntegral :: Int16 -> Double) . (fromIntegral :: Word16 -> Int16)) arr0
	let landscape = (height><width) $ arr0
	let jitter = randoms $ mkStdGen 53
	let scale = 2.5 --Tämä on viivojen lähtöpaikkojen keskimääräinen välinen etäisyys
	let [miny, maxy, minx, maxx] = map fromIntegral [1, height-2, 1, width-2]
	let (numpointsy, numpointsx) = (floor $ (maxy-miny)/scale, floor $ (maxx-minx)/scale)
	let startinggrid = [(y,x) | y <- [0..numpointsy-1], x<-[0..numpointsx-1]]
	let startingpoints = zipWith (\(y,x) (jy,jx) -> 
		( miny + scale * (fromIntegral y + jy)
		, minx + scale * (fromIntegral x + jx)
		)) startinggrid jitter
	--                                 v   gradient ascentin askelpituus
	let ascents = map (gradientAscent 1.0 landscape) startingpoints
	let bitmapmultiplier = 1::Int --Tekee bitmapista n kertaa isomman 
	s <- createImageSurface FormatARGB32 (bitmapmultiplier*width) (bitmapmultiplier*height)
	renderWith s $ do
		setSourceRGBA 1 1 1 0.0
		paint
	mapM_ (drawLine (fromIntegral bitmapmultiplier) s) ascents
	surfaceWriteToPNG s "mok.png"

hajoita :: Int -> [a] -> [a]
hajoita n lasku = (foldr1 `par` list) `pseq` concat list
	where
	size = floor $ fromIntegral (length lasku) / fromIntegral n
	list = chunk (n-1) lasku
	chunk 0 l = [l]
	chunk n l = let (a,b) = splitAt size lasku in a : chunk (n-1) b

drawLine mult s ((x1,y1,z1):loput@((x2,y2,z2):_)) = do
	renderWith s $ do
		setLineWidth 0.5 --Viivan leveys
		setLineCap LineCapRound
		setSourceRGB c c c
		moveTo (x1*mult) (y1*mult)
		lineTo (x2*mult) (y2*mult)
		stroke
	drawLine mult s loput
	where
	--                             pienempi=tummempi
	c = 1.0 - ((atan2 (abs(z1-z2)) 0.125)/(pi/2))
drawLine mult s _ = return ()
