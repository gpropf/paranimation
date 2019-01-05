module Paranimate.Paranimate where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear
import Data.Map
import Control.Monad
import Data.Maybe
import Data.List
import Number.Complex
import Algebra.Ring( C )
import Text.Printf
import System.Random


data BV a =
  BV a
  | BVC (Number.Complex.T a)
  | BVI Int
  deriving (Show)

BV x `nplus` BV y = BV (x+y)
BV x `nplus` BVI y = BVI $ round (x + (fromIntegral y))
BVI x `nplus` BVI y = BVI (x+y)
BVC x `nplus` BVC y = BVC $ (real x + real y) +: (imag x + imag y)

BV x `nminus` BV y = BV (x-y)
BVI x `nminus` BVI y = BVI (x-y)
BVI x `nminus` BV y = BVI (round $ (fromIntegral x) - y)
BVC x `nminus` BVC y = BVC $ (real x - real y) +: (imag x - imag y)

BV x `mulByScalar` s = BV (x * s)
BVI x `mulByScalar` s = BV ((fromIntegral x) * s)
BVC x `mulByScalar` s = BVC (scale s x)

{- -}
data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show)


{- Viewport: A simple way to define the virtual space occupied by the frame. Since we currently use the Rasterific graphics framework the drawing primitives use pixel coordinates. For most animations, though, there is a need to use an abstract cartesian coordinate system centered on 0,0. A good example is the Mandelbrot set. Almost everything interesting this classic fractal happens in the square defined by -2,-2 and 2,2. We need a simple way to map coordinates like this to pixel-space. This type does that. As an example, let's say we wanted to draw a Mandelbrot set on a 1000x800 pixel canvas with the boundaries as described above. Note that the frame is 4 units wide and 4 tall in the Mandelbrot space, the 0,0 point in the pixel plane is analagous to the -2,-2 point in the Mandelbrot space, and the 1000,800 point in pixel space is at 2,2 in Mandelbrot space. This means that upperLeft is -2,-2 and scaleFactors is (1000 / 4),(800 / 4). Zoom and pan effects can be created by altering upperLeft and/or scaleFactors as needed. -}

data Viewport = Viewport { upperLeft :: Vec2 Double, scaleFactors :: Vec2 Double} deriving (Show)

{- ModuleWorkers: A convenience type that contains the two elements that
 essentially define the core of a module. Passing this structure
 allows a single argument to be used in running a module. -}

data ModuleWorkers = ModuleWorkers { pHash :: Data.Map.Map [Char] [(Double, BV Double)],
                                     makeFrameFn :: Data.Map.Map [Char] [(Double, BV Double)] -> StdGen -> Double -> Codec.Picture.Image Codec.Picture.PixelRGBA8
                                   }

(|+) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(|+) (Vec2 x y) (Vec2 u v) = Vec2 (x+u) (y+v)


(|-) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(|-) (Vec2 x y) (Vec2 u v) = Vec2 (x-u) (y-v)


(|*) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(|*) (Vec2 a b) (Vec2 c d) = Vec2 (a * c - b * d) (a * d + b * c)


vec2Scale :: Num a => a -> Vec2 a -> Vec2 a
vec2Scale s v = Vec2 (s * (x v)) (s * (y v))


vec2abs :: Floating a => Vec2 a -> a
vec2abs (Vec2 a b) = sqrt ((a*a) + (b*b))

vec2toV2 :: (Real a1, Fractional a2) => Vec2 a1 -> Graphics.Rasterific.Linear.V2 a2
vec2toV2 (Vec2 a b) = V2 (realToFrac a) (realToFrac b)

{- -}
viewport2abs :: Fractional a => Viewport -> Vec2 Double -> Graphics.Rasterific.Linear.V2 a
viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = ((y $ upperLeft vp) - y p) * (y $ scaleFactors vp)
  in
    vec2toV2 $ Vec2 px py


vec2fromComplex :: Number.Complex.T a -> Vec2 a
vec2fromComplex c = Vec2 (real c) (imag c)

{- -}
linearInterpolate :: (RealFrac a, Fractional a, Algebra.Ring.C a) => (a, BV a) -> (a, BV a) -> a -> BV a
linearInterpolate (t1, v1) (t2, v2) t =
  let rise = v1 `nminus` v2
      run = t1 - t2
      m = rise `mulByScalar` (1/run)
      b = v1 `nminus` (m `mulByScalar` t1)
      boxedVal = ((m `mulByScalar` t) `nplus` b)
  in
    boxedVal

{- -}    
interpolatedValue
  :: (Ord a, Ord k) =>
     ((a, b) -> (a, b) -> a -> t)
     -> a -> k -> Data.Map.Map k [(a, b)] -> t
interpolatedValue interpFn t keyStr hash =
  let curvePoints = fromMaybe [] $ Data.Map.lookup keyStr hash
  in
    valueOnCurve interpFn curvePoints t

{- -}
valueOnCurve
  :: Ord a => ((a, b) -> (a, b) -> a -> t) -> [(a, b)] -> a -> t
valueOnCurve interpFn curvePoints t =
  let (lts,gts) = Data.List.partition (\(tn,vn) -> t >= tn) curvePoints
      
  in
    interpFn (last lts) (head gts) t



makeImageList
  :: (Map [Char] [(Double, BV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Data.Map.Map [Char] [(Double, BV Double)]
  -> [StdGen] -> [Double]
  -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]
makeImageList makeFrameFn paramHash gs rangeT
  = Prelude.zipWith (makeFrameFn paramHash) gs rangeT


writeImageList
  :: (Map [Char] [(Double, BV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Map [Char] [(Double, BV Double)]
  -> [Char] -> [StdGen] -> [Double] -> [IO ()]
writeImageList makeFrameFn paramHash baseFilename gs rangeT =
  let lenRangeT = length rangeT
      numZeros = length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
  in
    zipWith
    (\fname image -> Codec.Picture.writePng fname image)
    (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes)
    (makeImageList makeFrameFn paramHash gs rangeT)

