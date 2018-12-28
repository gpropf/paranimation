-- {-# LANGUAGE AllowAmbiguousTypes, MultiParamTypeClasses #-}



module ParamUtil where

import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Number.Complex
import Graphics.Rasterific.Linear
import Algebra.Ring( C )
import Data.List
import Data.Map
import Data.Maybe
import Paranimate
--import Params
import ModuleTemplate
import Text.Printf


{-
makeImageList
  :: Data.Map.Map [Char] [(Double, BV Double)]
     -> [Double]
     -> [Codec.Picture.Image Codec.Picture.PixelRGBA8] -}
makeImageList makeFrameFn paramHash rangeT = Prelude.map (makeFrameFn paramHash) rangeT


{-
writeImageList
  :: Data.Map.Map [Char] [(Double, BV Double)]
     -> [Char] -> [Double] -> [IO ()] -}
writeImageList makeFrameFn paramHash baseFilename rangeT =
  let lenRangeT = length rangeT
      numZeros = length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
      --makeFrameFn = ModuleTemplate.makeFrame
  in
    zipWith (\fname image -> Codec.Picture.writePng fname image) (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes) (makeImageList makeFrameFn paramHash rangeT)

