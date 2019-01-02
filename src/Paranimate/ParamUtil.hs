-- {-# LANGUAGE AllowAmbiguousTypes, MultiParamTypeClasses #-}



module Paranimate.ParamUtil where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex
import Graphics.Rasterific.Linear
import Algebra.Ring( C )
import Data.List
import Data.Map
import Data.Maybe
import Paranimate.Paranimate
import Text.Printf
import System.Random


makeImageList
  :: (Map [Char] [(Double, BV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Data.Map.Map [Char] [(Double, BV Double)]
  -> [StdGen] -> [Double]
  -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]
makeImageList makeFrameFn paramHash gs rangeT
  = Prelude.zipWith (makeFrameFn paramHash) gs rangeT


writeImageList
  :: (Map [Char] [(Double, BV Double)] -> StdGen -> Double -> Image PixelRGBA8)
  -> Map [Char] [(Double, BV Double)]
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

