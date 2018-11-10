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
import Data.List
import Number.Complex
import Params

--data BoxedVal a = BoxedVal a deriving (Show)

getParam :: String -> Map String [(Double, [Double])]  -> Maybe [(Double, [Double])]
getParam p params = do
  localP <- Data.Map.lookup p params
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

  
makeImageList params rangeT = Prelude.map makeFrame2 $ Prelude.map (getParamSnaphot2 params) rangeT

writeImageList params baseFilename rangeT = zipWith (\fname image -> Codec.Picture.writePng fname image) (Prelude.map (\t -> baseFilename ++ "-" ++ show t ++ ".png") rangeT) (makeImageList params rangeT)


-- drawTrack :: [PointMass]

