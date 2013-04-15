import Control.Monad
import Control.Monad.State
import Data.Binary
import Data.Binary.Get
import Data.Int
import Data.List
import Data.List.Split
import Data.Word
import Graphics.Rendering.Cairo hiding (Matrix)
import Numeric.LinearAlgebra
import qualified Data.ByteString.Lazy as BS
import System.Random
import Text.Printf
import Bicubic
import BinaryExtras
import RandomExtras
import Types

main = do
	contents <- BS.readFile "tesannwyn.raw"
	let height = 2816
	let width = 2688
	let arr0 = map tesToDouble $ runGet (getMany $ height*width) contents
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
	surfaceWriteToPNG s "vv.png"

drawLine mult s ((x1,y1,z1):loput@((x2,y2,z2):_)) = do
	renderWith s $ do
		setSourceRGB c c c
		--Viivan leveys
		setLineWidth 3
		setLineCap LineCapRound
		moveTo (x1*mult) (y1*mult)
		lineTo (x2*mult) (y2*mult)
		stroke
	drawLine mult s loput
	where
	c = 1.0 - ((atan2 (abs(z1-z2)) 8)/(pi/2))
drawLine mult s _ = return ()

