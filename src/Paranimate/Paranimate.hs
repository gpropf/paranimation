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
import Control.Parallel.Strategies
import Linear as L


data IV a =
  IV a
  | IVC (Number.Complex.T a)
  | IVI Int
  deriving (Show)

IV x `ivplus` IV y = IV (x+y)
IV x `ivplus` IVI y = IVI $ round (x + (fromIntegral y))
IVI x `ivplus` IVI y = IVI (x+y)
IVC x `ivplus` IVC y = IVC $ (real x + real y) +: (imag x + imag y)

IV x `ivminus` IV y = IV (x-y)
IVI x `ivminus` IVI y = IVI (x-y)
IVI x `ivminus` IV y = IVI (round $ (fromIntegral x) - y)
IVC x `ivminus` IVC y = IVC $ (real x - real y) +: (imag x - imag y)

IV x `ivscale` s = IV (x * s)
IVI x `ivscale` s = IV ((fromIntegral x) * s)
IVC x `ivscale` s = IVC (scale s x)

{- -}
data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show)


{- Viewport: A simple way to define the virtual space occupied by the frame. Since we currently use the Rasterific graphics framework the drawing primitives use pixel coordinates. For most animations, though, there is a need to use an abstract cartesian coordinate system centered on 0,0. A good example is the Mandelbrot set. Almost everything interesting this classic fractal happens in the square defined by -2,-2 and 2,2. We need a simple way to map coordinates like this to pixel-space. This type does that. As an example, let's say we wanted to draw a Mandelbrot set on a 1000x800 pixel canvas with the boundaries as described above. Note that the frame is 4 units wide and 4 tall in the Mandelbrot space, the 0,0 point in the pixel plane is analagous to the -2,-2 point in the Mandelbrot space, and the 1000,800 point in pixel space is at 2,2 in Mandelbrot space. This means that upperLeft is -2,-2 and scaleFactors is (1000 / 4),(800 / 4). Zoom and pan effects can be created by altering upperLeft and/or scaleFactors as needed. -}

data Viewport = Viewport { upperLeft :: Vec2 Double, scaleFactors :: Vec2 Double} deriving (Show)

{- ModuleWorkers: A convenience type that contains the two elements that
 essentially define the core of a module. Passing this structure
 allows a single argument to be used in running a module. -}

data ModuleWorkers = ModuleWorkers { pHash :: Data.Map.Map [Char] [(Double, IV Double)],
                                     makeFrameFn :: Data.Map.Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Codec.Picture.Image Codec.Picture.PixelRGBA8
                                   }
{-<<< Matrix code -}

{-<<< For Testing -}

{-
ul = L.V2 (-2.0) 2.0
lr = L.V2 2.0 (-2.0)
ll = L.V2 (-2.0) (-2.0)

scrul = L.V2 0.0 0.0
scrlr = L.V2 800.0 600.0
scrll = L.V2 0 600.0
-}
{-
ul = (-2.0) +: 2.0
lr = 2.0 +: (-2.0)
ll = (-2.0) +: (-2.0)

scrul = 0.0 +: 0.0
scrlr = 800.0 +: 600.0
scrll = 0 +: 600.0
-}

{-
inM = affineMatrix33 ul lr
outM = affineMatrix33 scrul scrlr
-}
{- For Testing >>>-}
{-
affineMatrix33 (L.V2 x1 y1) (L.V2 x2 y2) =  
  let v1 = L.V3 x1 y1 1
      v2 = L.V3 x2 y2 1
      v3 = L.V3 1 1 1
  in
    L.V3 v1 v2 v3
-}

transformationMatrix (ul1,lr1,ll1) (ul2,lr2,ll2) =
  let inM = L.V3 (t ul1) (t lr1) (t ll1)
      outM = L.V3 (t ul2) (t lr2) (t ll2)
  in
    (inv33 inM) !*! outM
  where
    t = lv2_lv3 . cplx_lv2

lv2_lv3 (L.V2 x y) = L.V3 x y 1
vec2_lv2 (Vec2 x y) = L.V2 x y
lv3_glv2 (L.V3 x y _) = Graphics.Rasterific.Linear.V2 x y

cplx_lv2 c =
  let r = real c
      i = imag c
  in
    L.V2 r i 

projectPtWithMatrix m p =
  let p3d = (lv2_lv3 . vec2_lv2) p
  in
    fmap realToFrac $ lv3_glv2 $ p3d *! m
                   
--transformationMatrix inM outM = (inv33 inM) !*! outM
{-<<< Matrix code >>>-}



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
vec2toV2 (Vec2 a b) = Graphics.Rasterific.Linear.V2 (realToFrac a) (realToFrac b)

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
linearInterpolate :: (RealFrac a, Fractional a, Algebra.Ring.C a) => (a, IV a) -> (a, IV a) -> a -> IV a
linearInterpolate (t1, v1) (t2, v2) t =
  let rise = v1 `ivminus` v2
      run = t1 - t2
      m = rise `ivscale` (1/run)
      b = v1 `ivminus` (m `ivscale` t1)
      boxedVal = ((m `ivscale` t) `ivplus` b)
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
    case gts of
      [] ->
        let (p:ps) = reverse lts
        in
          interpFn p (head ps) t
      gts' -> interpFn (last lts) (head gts') t



makeImageList
  :: (Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Data.Map.Map [Char] [(Double, IV Double)]
  -> [StdGen] -> [Double]
  -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]
makeImageList makeFrameFn paramHash gs rangeT
  = let fs = Prelude.zipWith (makeFrameFn paramHash) gs rangeT
        fs' = fs `using` parList rpar
    in
      fs'


writeImageList
  :: (Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Map [Char] [(Double, IV Double)]
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

