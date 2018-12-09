-- {-# LANGUAGE AllowAmbiguousTypes, MultiParamTypeClasses #-}



module ParamUtil where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex
import Graphics.Rasterific.Linear
import Algebra.Ring( C )
{-
data BoxedVal a = BoxedInt Int
  | BoxedDouble Double
  | BoxedList [a]
  | BoxedVec2 (Vec2 a) deriving (Show)
-}

data BV a =
  BV a
  | BVC (Number.Complex.T a)
  | BVI Int
  deriving (Show)

BV x `nplus` BV y = BV (x+y)
BVI x `nplus` BVI y = BVI (x+y)
--BVI x `nplus` BVI y = BVI (x+y)
BVC x `nplus` BVC y = BVC $ (real x + real y) +: (imag x + imag y)

BV x `nminus` BV y = BV (x-y)
BVI x `nminus` BVI y = BVI (x-y)
BVC x `nminus` BVC y = BVC $ (real x - real y) +: (imag x - imag y)

BV x `mulByScalar` s = BV (x * s)
BVI x `mulByScalar` s = BV ((fromIntegral x) * s)
BVC x `mulByScalar` s = BVC (scale s x)



--interpolate2 :: (Fractional a, Algebra.Ring.C a) => (a, BV a) -> (a, BV a) -> a -> BV a
interpolate2 (t1, v1) (t2, v2) t =
  let rise = v1 `nminus` v2
      run = t1 - t2
      m = rise `mulByScalar` (1/run)
      b = v1 `nminus` (m `mulByScalar` t1)
      boxedVal = ((m `mulByScalar` t) `nplus` b)
  in
    boxedVal

    
  

data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show)

data Viewport = Viewport { upperLeft :: Vec2 Double, scaleFactors :: Vec2 Double} deriving (Show)

-- - End global vars and data structures
-- --------------------------------------------------

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


viewport2abs :: Fractional a => Viewport -> Vec2 Double -> Graphics.Rasterific.Linear.V2 a
viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = ((y $ upperLeft vp) - y p) * (y $ scaleFactors vp)
  in
    vec2toV2 $ Vec2 px py


vec2fromComplex :: Number.Complex.T a -> Vec2 a
vec2fromComplex c = Vec2 (real c) (imag c)


i3 = 3 :: Int
i5 = 5 :: Int
i4 = 4 :: Int
i7 = 7 :: Int

f3 = 3 :: Float
f6 = 6 :: Float 


t1 = 1.0
t2 = 5.0

v1 = Vec2 1 0
v2 = Vec2 0 1

c1 = 4 +: 0
c2 = 0 +: 4

pFloat = interpolate2 (t1, BV f3) (t2, BV f6) 4.0
pCmplx = interpolate2 (t1, BVC c1) (t2, BVC c2) 3.0
pInt = interpolate2 (t1, BVI i3) (t2, BVI i7) 3.0
--pInt = interpolate2 (t1, BV i3) (t2, BV i7) 4.0
{- nplus :: (Num t) => BV t -> BV t -> BV t  
n1 `nplus` n2 =
  case (n1,n2) of
    (BV c1, BV c2) -> BV $ (real c1 + real c2) +: (imag c1 + imag c2)
    (_,_) -> BV 0
   -} 

{-
interpolate2 :: (Num a) => (Double, BV a) -> (Double, BV a) -> Double -> BV a
interpolate2 (t1, BV v1) (t2, BV v2) t =
  let rise = fromIntegral v2 - fromIntegral v1
      run = t2 - t1
      m = rise / run
      b = fromIntegral v1 - m * t1
  in
    
    BV $ fromIntegral (m * t + b)
    -}
{-
interpolate :: (Num a, Integral a) => (Double, BoxedVal a) -> (Double, BoxedVal a) -> Double -> BoxedVal Double
interpolate (t1, v1) (t2,v2) t =
  case (v1,v2) of
    (BoxedDouble v1d, BoxedDouble v2d) ->
      let rise = v2d - v1d
          m = rise / run
          b = v1d - m * t1
      in
        BoxedDouble (m * t + b)
    (BoxedInt v1i, BoxedInt v2i) ->
      let rise = fromIntegral $ v2i - v1i
          m = rise / run
          b = fromIntegral v1i - m * t1
      in
        BoxedInt (round (m * t + b))
    (BoxedVec2 v1, BoxedVec2 v2) ->
      let v1x = fromIntegral $ x v1
          v1y = fromIntegral $ y v1
          v2x = fromIntegral $ x v2
          v2y = fromIntegral $ y v2
          riseX = v2x - v1x
          riseY = v2y - v1y
          mx = riseX / run
          my = riseY / run
          bx = v1x - mx * t1
          by = v1y - my * t1
      in
        BoxedVec2 (Vec2 (mx * t + bx) (my * t + by))
    (_,_) -> BoxedInt 0
  where
    run = t2 - t1

               
-}