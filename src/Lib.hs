module Lib where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
--import Data.Map
import Data.Map
--import qualified Data.Map as Map
import Control.Monad
import Data.Maybe

--data BoxedVal a = BoxedVal a deriving (Show)
data BoxedVal a = BoxedInt Int | BoxedFloat Double deriving (Show)

data Vec2 a = Vec2 { x :: a, y :: a } deriving (Show)


--instance Num a => Semigroup (Vec2 a) where
(|+) (Vec2 x y) (Vec2 u v) = Vec2 (x+u) (y+v)
(|-) (Vec2 x y) (Vec2 u v) = Vec2 (x-u) (y-v)
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

getParam :: String -> Map String [(Double, [Double])]  -> Maybe [(Double, [Double])]
getParam p params = do
  localP <- lookup p params
  return localP

getPolynomialCoeffs :: Double -> Map String [(Double, [Double])] -> Map String (Maybe [Double])
getPolynomialCoeffs t params = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (getCoefficients t) (getParam k params)) params

getParamSnaphot :: (Monad m, Num r) => r -> Map k (m [r]) -> Map k (m r)
getParamSnaphot t params = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (evaluatePolynomial t) v) params

getParamSnaphot2 params t = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (evaluatePolynomial t) v) $ getPolynomialCoeffs t params

evaluatePolynomial :: Num a => a -> [a] -> a
evaluatePolynomial t coeffs = sum $ Prelude.map (\(i,c) -> c*t^i) $ Prelude.zip [0..] coeffs


getCoefficients :: Ord t => t -> [(t, [a])] -> [a]
getCoefficients t coeffThresholds =
  if Prelude.null coeffThresholds
     then []
  else let c:cs = coeffThresholds
           (threshold, coeffList) = c
       in
         if t <= threshold then coeffList
         else
           getCoefficients t cs

  
makeImageList params rangeT = Prelude.map makeFrame $ Prelude.map (getParamSnaphot2 params) rangeT

writeImageList params baseFilename rangeT = zipWith (\fname image -> writePng fname image) (Prelude.map (\t -> baseFilename ++ "-" ++ show t ++ ".png") rangeT) (makeImageList params rangeT)

{-
someFunc :: IO ()
someFunc = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      recColor = PixelRGBA8 0xFF 0x53 0x73 255
      img = renderDrawing 400 200 white $
        withTexture (uniformTexture drawColor) $
        do
          fill $ circle (V2 0 0) 30
          stroke 4 JoinRound (CapRound, CapRound) $
            circle (V2 400 200) 40
          withTexture (uniformTexture recColor) .
            fill $ rectangle (V2 100 100) 200 100
  writePng "yourimage.png" img
-}                   
makeFrame :: Map String (Maybe Double) -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame params = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      recColor = PixelRGBA8 0xFF 0x53 0x73 255
      maybex  = fromJust $ Data.Map.lookup "r" params
      xDbl = fromJust maybex
      img = renderDrawing 400 200 white $
        withTexture (uniformTexture drawColor) $
        do
          fill $ circle (V2 0 0) 30
          stroke 4 JoinRound (CapRound, CapRound) $
            circle (V2 400 200) (40 + realToFrac xDbl)
          withTexture (uniformTexture recColor) .
            fill $ rectangle (V2 100 100) 200 100
    in
    img
--  writePng "yourimage.png" img



viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = (y p - (y $ upperLeft vp)) * (y $ scaleFactors vp)
  in
    Vec2 px py



