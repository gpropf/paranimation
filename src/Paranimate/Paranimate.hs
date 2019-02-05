--{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies
--    , FlexibleContexts, FlexibleInstances, AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts, DeriveGeneric, FlexibleInstances #-}
--, DataKinds #-}



module Paranimate.Paranimate where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear as GL
import Graphics.Rasterific.Transformations as GT
import Data.Map
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Number.Complex
import Algebra.Ring( C )
import Text.Printf
import System.Random
import Control.Parallel.Strategies
import Linear as L
import Control.Monad.Free.Church( F, fromF )
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy



{- ModuleWorkers: A convenience type that contains the two elements that
 essentially define the core of a module. Passing this structure
 allows a single argument to be used in running a module. -}

data ModuleWorkers = ModuleWorkers { pHash :: Data.Map.Map [Char] [(Double, IV Double)],
                                     makeFrameFn :: Data.Map.Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Codec.Picture.Image Codec.Picture.PixelRGBA8
                                   }

type TransformMatrix = L.V3 (L.V3 Double)

everyf n [] = []
everyf n as  = Data.List.head as : everyf n (Data.List.drop n as)

{-<<< Matrix and vector operation code: -}                          
transformationMatrix (ul1,lr1,ll1) (ul2,lr2,ll2) =
  let inM = L.V3 (t ul1) (t lr1) (t ll1)
      outM = L.V3 (t ul2) (t lr2) (t ll2)
  in
    (inv33 inM) !*! outM
  where
    t = lv2_lv3 . cplx_lv2

lv2_lv3 (L.V2 x y) = L.V3 x y 1
vec2_lv2 (Vec2 x y) = L.V2 x y
lv3_glv2 (L.V3 x y _) = GL.V2 (realToFrac x) (realToFrac y)

cplx_lv2 c =
  let r = real c
      i = imag c
  in
    L.V2 r i 

{- toRasterificTransform: Warning! Untested! -}
toRasterificTransform (L.V3 (L.V3 a b _) (L.V3 c d _) (L.V3 e f _)) = 
  GT.Transformation a c e b d f


projectPtWithMatrixFmap m p =
  let p3d = (lv2_lv3 . vec2_lv2) p
  in
    fmap realToFrac $ lv3_glv2 $ p3d *! m
                   

projectPtWithMatrix m p = 
  let p3d = (lv2_lv3 . vec2_lv2) p
  in
    lv3_glv2 $ p3d *! m

{- prefix: This is the "magic" string that prefixes all configuration
 values that we reserve for special use here. This is intended to make
 namespace collisions with user defined values very unlikely. -}
prefix = "PARANIMATE_"


{- parametricTransform: Takes the paramHash map that is passed to
 makeFrameFn and pulls out several "magic" values prefixed with the
 global 'prefix' string above. The first 3 are the vertices of a
 triangle defining the viewport in the virtual space. A typical
 Mandelbrot set for instance uses a window centered on the origin
 extending for 2 units in the x and y directions making a 4x4 unit
 window. The defining triangle for this would probably be ul: (-2,2),
 lr: (2,-2), and ll: (-2,-2). The final "magic" value is called
 <prefix>geom which simply defines the lower right corner of the
 images in pixels. A 1920x1080 image is thus just a single value of
 lr: (1920,1080). From this we derives the other 2 corners of the
 image triangle. The ul value is always (0,0), the ll value is, in
 this case, (0,1080). Once these triangles are defined we do some
 matrix algebra to derive a transformation matrix that can be used to
 map things from virtual coordinates (viewport coords) to real pixel
 values. -}
parametricTransform :: (RealFrac a, Algebra.Ring.C a) =>
  Data.Map.Map [Char] [(a, IV a)]
  -> a -> L.V3 (L.V3 a)
parametricTransform paramHash t =
  let (IVC ul) = prefixedValue "ul"
      (IVC lr) = prefixedValue "lr"
      (IVC ll) = prefixedValue "ll"
--      (IVC imgul) = prefixedValue "imgul"
      (IVC imglr) = prefixedValue "geom"
--      (IVC imgll) = prefixedValue "imgll"
      imgll = 0 +: (imag imglr)
      imgul = 0 +: 0
  in
    transformationMatrix (ul,lr,ll) (imgul,imglr,imgll)
    where
      prefixedValue valName = interpolatedValue
                              linearInterpolate t (prefix ++ valName)
                              paramHash
      
  



{- Matrix and vector operation code >>>---------------------------}


{- <<<- Vec2: My data type for 2D vectors. My initial motivation was to make
 something that could be easily mapped over ints, floats, doubles or
 other types as well as serialized for configuration purposes. I'm not
 sure it really serves a purpose anymore but it's still in use so I
 have to keep it for now. There are operators defined below that do
 the usual mathematical things you need vectors to do.
-}

data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show, Generic)

instance ToJSON (Vec2 Int)
instance FromJSON (Vec2 Int)
instance ToJSON (Vec2 Double)
instance FromJSON (Vec2 Double)

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

vec2toV2 :: (Real a1, Fractional a2) => Vec2 a1 -> GL.V2 a2
vec2toV2 (Vec2 a b) = GL.V2 (realToFrac a) (realToFrac b)


vec2fromComplex :: Number.Complex.T a -> Vec2 a
vec2fromComplex c = Vec2 (real c) (imag c)

{- Vec2 ->>> --------------------------------------------}


{- <<<- IV: Literally "interpolatable value". This is meant to represent
 values that can be paramaterized over some other value. Essentially
 the idea is that these are the dependent values with paired
 independent values of some (perhaps other) type. Animations are
 driven by providing a list of such values paired with their
 independent values. Essentially you provide a list of x,y pairs where
 the x value is usually some kind of "time" parameter. By providing a
 specific value of the 'x' type the system can then interpolate the
 'y' types to give a specific value that is then used in some aspect
 of the animation.
-}
data IV a =
  IV a
  | IVC (Number.Complex.T a)
  | IVI Int
  deriving (Generic, Show)


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
IVC x `ivscale` s = IVC (Number.Complex.scale s x)



{- linearInterpolate : Take two x,y pairs and a single value of type x
and produce an linearly interpolated value of type y. Example: use
arguments (4,10), (8,50), and t = 6. The calculated value is 30 since
rise/run = (50-10)/(8-4) = 40/4 = 10, 6 - 4 = 2, and 2 * 10 + 10 =
30. This is essentially just a simple solver for the linear equation
of the form y = mx + b. -}
linearInterpolate :: (RealFrac a, Fractional a, Algebra.Ring.C a) => (a, IV a) -> (a, IV a) -> a -> IV a
linearInterpolate (t1, v1) (t2, v2) t =
  let rise = v1 `ivminus` v2
      run = t1 - t2
      m = rise `ivscale` (1/run)
      b = v1 `ivminus` (m `ivscale` t1)
      boxedVal = ((m `ivscale` t) `ivplus` b)
  in
    boxedVal

{- interpolatedValue: This allows us to use the provided interpolator
 function 'interpFn', an independent variable value 't', a string
 representing the name of the value we want, and the Data.Map.Map of
 interpolation lists. In theory we can use any arbitrary interpolator
 function including things like bezier curves. Right now there is only
 the linear interpolator. -}    
interpolatedValue
  :: (Ord a, Ord k) =>
     ((a, b) -> (a, b) -> a -> b)
     -> a -> k -> Data.Map.Map k [(a, b)] -> b
interpolatedValue interpFn t keyStr hash =
  let curvePoints = fromMaybe [] $ Data.Map.lookup keyStr hash
  in
    valueOnCurve interpFn curvePoints t

{- valueOnCurve: workhorse function for interpolatedValue function. This
 where interpFn is actually run to find the in-between value from a
 pair of endpoints and a single independent variable value. -}
valueOnCurve
  :: Ord a => ((a, b) -> (a, b) -> a -> b) -> [(a, b)] -> a -> b
valueOnCurve interpFn curvePoints t =
  -- FIXME! This is a mess and we need a better way to handle single
  -- data pairs and independent values outside the curvePoints list.
  if Data.List.length curvePoints < 2 then
    let (iv, dv) = Data.List.head curvePoints
    in
      dv
  else
    let (lts,gts) = Data.List.partition (\(tn,vn) -> t >= tn) curvePoints
    in
      case gts of
        [] ->
          let (p:ps) = Data.List.reverse lts
          in
            interpFn p (Data.List.head ps) t
        gts' -> interpFn (Data.List.last lts) (Data.List.head gts') t

{- makeImageList: This func is used by the main program to build the
 series of frames using the module-specific makeFrameFn, the
 module-specific paramHash and a list of random number generators and
 a list of independant variable values. The RNGs are provided as a
 convenience to the module writer and also allow the modules to remain
 purely functional while making use of (pseudo-) random
 processes. This func is evaluated using Haskell's
 Control.Parallel.Strategies module so as to create the frames in
 parallel using multiple cores. I have found this approach to be
 roughly 7 times faster than simple serial processing on my 6-core
 Intel Linux machine. To disable it simply substitute fs for fs' as
 the return value. -}
makeImageList
  :: (Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Data.Map.Map [Char] [(Double, IV Double)]
  -> [StdGen] -> [Double]
  -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]
makeImageList makeFrameFn paramHash gs rangeT
  = let fs = Data.List.zipWith (makeFrameFn paramHash) gs rangeT
        fs' = fs `using` parList rpar
    in
      fs'

{- writeImageList: This is where the program really starts. This is
 called from main and creates the RNGs and the image filenames and
 then calls makeImageList above. It then writes the resulting image
 files to disk. It passes makeFrameFn and paramHash on through to
 makeImageList. Doing this allow us to isolate impure code (filesystem
 access and RNG creation) here. -}
writeImageList
  :: (Map [Char] [(Double, IV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Map [Char] [(Double, IV Double)]
  -> [Char] -> [StdGen] -> [Double] -> [IO ()]
writeImageList makeFrameFn paramHash baseFilename gs rangeT =
  let lenRangeT = Data.List.length rangeT
      numZeros = Data.List.length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
  in
    Data.List.zipWith
    (\fname image -> Codec.Picture.writePng fname image)
    (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes)
    (makeImageList makeFrameFn paramHash gs rangeT)


data ModuleConfig = ModuleConfig {
  vecMap :: Data.Map.Map [Char] [(Double, Vec2 Double)],
  dblMap :: Data.Map.Map [Char] [(Double, Double)]
  } deriving (Show, Generic)


instance ToJSON ModuleConfig
instance FromJSON ModuleConfig


testMC = ModuleConfig
  (Data.Map.fromList ([("v1", [(0.0, Vec2 3.0 4.0),(10.0, Vec2 13.0 14.0)]),
                       ("v2", [(0.0, Vec2 6.0 7.0),(11.0, Vec2 15.0 19.0)])]))

  (Data.Map.fromList ([("foo", [(0.0,4.5),(20.0,3.14159)]),
                       ("bar", [(0.0,8.5),(21.0,9.14159)])]))

  
