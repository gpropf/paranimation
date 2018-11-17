module Util where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex
import Graphics.Rasterific.Linear

data BoxedVal a = BoxedInt Int | BoxedDouble Double | BoxedList [a] deriving (Show)

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


v2abs :: Floating a => Vec2 a -> a
v2abs (Vec2 a b) = sqrt ((a*a) + (b*b))


toV2 :: (Real a1, Fractional a2) => Vec2 a1 -> Graphics.Rasterific.Linear.V2 a2
toV2 (Vec2 a b) = V2 (realToFrac a) (realToFrac b)


viewport2abs :: Fractional a => Viewport -> Vec2 Double -> Graphics.Rasterific.Linear.V2 a
viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = ((y $ upperLeft vp) - y p) * (y $ scaleFactors vp)
  in
    toV2 $ Vec2 px py


complex2Vec2 :: Number.Complex.T a -> Vec2 a
complex2Vec2 c = Vec2 (real c) (imag c)


interpolate (t1, v1) (t2,v2) t =
  case (v1,v2) of
    (BoxedDouble v1d, BoxedDouble v2d) -> BoxedDouble (v2d - v1d)
    (BoxedInt v1i, BoxedInt v2i) -> BoxedInt (v2i - v1i)
    (_,_) -> BoxedInt 0

               
