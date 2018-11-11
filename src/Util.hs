module Util where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex

data BoxedVal a = BoxedInt Int | BoxedFloat Double deriving (Show)

data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show)


--instance Num a => Semigroup (Vec2 a) where
(|+) (Vec2 x y) (Vec2 u v) = Vec2 (x+u) (y+v)
(|-) (Vec2 x y) (Vec2 u v) = Vec2 (x-u) (y-v)
(|*) (Vec2 a b) (Vec2 c d) = Vec2 (a * c - b * d) (a * d + b * c)
vec2Scale s v = Vec2 (s * (x v)) (s * (y v))

v2abs (Vec2 a b) = sqrt ((a*a) + (b*b))
--  Vec2 x y - Vec2 u v = Vec2 (x-u) (y-v)
 {- Vec2 x y * Vec2 u v = Vec2 (x*u) (y*v)
  abs (Vec2 x y) = let h = sqrt (x*x + y*y)
                   in
                     Vec2 h h
-}
--    , ‘signum’, ‘fromInteger’, and (either ‘negate’ or ‘-’)


data Viewport = Viewport { upperLeft :: Vec2 Double, scaleFactors :: Vec2 Double} deriving (Show)

--params :: Data.Map String (BoxedVal Double) 
--params = fromList([("x", BoxedInt 100), ("y", BoxedInt 75), ("accel", BoxedFloat 9.81)])
-- params :: Map String [(Double, [BoxedVal a])]
-- params = fromList ([("x", [(0.0, [BoxedInt 1,BoxedInt 2,BoxedInt 3]), (1.0, [BoxedInt 10,BoxedInt 20,BoxedInt 30]), (1.0, [BoxedInt 100,BoxedInt 200,BoxedInt 300])]),("y", [(0.0, [BoxedInt 1,BoxedInt 2,BoxedInt 3]), (1.0, [BoxedInt 10,BoxedInt 20,BoxedInt 30]), (2.0, [BoxedInt 100,BoxedInt 200,BoxedInt 300])]), ("accel", [(0.0, [BoxedFloat 1,BoxedFloat 2,BoxedFloat 3]), (1.0, [BoxedFloat 10,BoxedFloat 20,BoxedFloat 30]), (2.0, [BoxedFloat 100,BoxedFloat 200,BoxedFloat 300])])])

--params :: Map String [(Double, [Double])]
--params = fromList ([("x", [(0.0, [1,2,3]), (1.0, [ 10, 20, 30]), (2.0, [ 100, 200, 300])]),("y", [(0.0, [ 1, 2, 3]), (1.0, [ 10, 20, 30]), (2.0, [ 101, 201, 301])]), ("accel", [(0.0, [ 1, 2, 3]), (1.0, [ 4, 5, 6]), (2.0, [ 10, 20, 30])])])


{- Example of how to use these functions:
 liftM (getCoefficients (1.4)) (getParam "accel" params)

mapWithKey (\k v -> liftM (getCoefficients 1.1) (getParam k params)) params


-}


toV2 (Vec2 a b) = V2 (realToFrac a) (realToFrac b)

viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = ((y $ upperLeft vp) - y p) * (y $ scaleFactors vp)
  in
    toV2 $ Vec2 px py

complex2Vec2 c = Vec2 (real c) (imag c)
