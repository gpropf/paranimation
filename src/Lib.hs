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
import Text.Printf

--data BoxedVal a = BoxedVal a deriving (Show)


polynomialSnapshot :: Double -> Map String [(Double, [Double])] -> Map String (Maybe [Double])
polynomialSnapshot t params = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (coeffDomains t) (Data.Map.lookup k params)) params


paramSnaphot :: Data.Map.Map String [(Double, [Double])] -> Double -> Data.Map.Map String (Maybe Double)
paramSnaphot params t = Data.Map.mapWithKey (\k v -> Control.Monad.liftM (evaluatePolynomial t) v) $ polynomialSnapshot t params

evaluatePolynomial :: Num a => a -> [a] -> a
evaluatePolynomial t coeffs = sum $ Prelude.map (\(i,c) -> c*t^i) $ Prelude.zip [0..] coeffs


coeffDomains :: Ord t => t -> [(t, [a])] -> [a]
coeffDomains t coeffThresholds =
  if Prelude.null coeffThresholds
     then []
  else let c:cs = coeffThresholds
           (threshold, coeffList) = c
       in
         if t <= threshold then coeffList
         else
           coeffDomains t cs


makeImageList :: Data.Map.Map String [(Double, [Double])] -> [Double] -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]  
makeImageList params rangeT = Prelude.map makeFrame $ Prelude.map (paramSnaphot params) rangeT


writeImageList :: Data.Map.Map String [(Double, [Double])] -> [Char] -> [Double] -> [IO ()]
writeImageList params baseFilename rangeT =
  let lenRangeT = length rangeT
      numZeros = length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
  in
    zipWith (\fname image -> Codec.Picture.writePng fname image) (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes) (makeImageList params rangeT)


-- drawTrack :: [PointMass]

