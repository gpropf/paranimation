module Util where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex
import Graphics.Rasterific.Linear

data BoxedVal a = BoxedInt Int
  | BoxedDouble Double
  | BoxedList [a]
  | BoxedVec2 (Vec2 a) deriving (Show)

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

               
