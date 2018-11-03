module Lib where

import Prelude hiding (lookup)
import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
--import Data.Map
import Data.Map
--import qualified Data.Map as Map
import Control.Monad

--data BoxedVal a = BoxedVal a deriving (Show)
data BoxedVal a = BoxedInt Int | BoxedFloat Double deriving (Show)

--params :: Data.Map String (BoxedVal Double) 
--params = fromList([("x", BoxedInt 100), ("y", BoxedInt 75), ("accel", BoxedFloat 9.81)])
-- params :: Map String [(Double, [BoxedVal a])]
-- params = fromList ([("x", [(0.0, [BoxedInt 1,BoxedInt 2,BoxedInt 3]), (1.0, [BoxedInt 10,BoxedInt 20,BoxedInt 30]), (1.0, [BoxedInt 100,BoxedInt 200,BoxedInt 300])]),("y", [(0.0, [BoxedInt 1,BoxedInt 2,BoxedInt 3]), (1.0, [BoxedInt 10,BoxedInt 20,BoxedInt 30]), (2.0, [BoxedInt 100,BoxedInt 200,BoxedInt 300])]), ("accel", [(0.0, [BoxedFloat 1,BoxedFloat 2,BoxedFloat 3]), (1.0, [BoxedFloat 10,BoxedFloat 20,BoxedFloat 30]), (2.0, [BoxedFloat 100,BoxedFloat 200,BoxedFloat 300])])])

params :: Map String [(Double, [Double])]
params = fromList ([("x", [(0.0, [1,2,3]), (1.0, [ 10, 20, 30]), (2.0, [ 100, 200, 300])]),("y", [(0.0, [ 1, 2, 3]), (1.0, [ 10, 20, 30]), (2.0, [ 101, 201, 301])]), ("accel", [(0.0, [ 1, 2, 3]), (1.0, [ 4, 5, 6]), (2.0, [ 10, 20, 30])])])


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

getParamSnaphot2 t params = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (evaluatePolynomial t) v) $ getPolynomialCoeffs t params

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
                   

makeFrame params = do
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
