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
import Lib
import Params
import Text.Printf



makeImageList2
  :: Data.Map.Map [Char] [(Double, BV Double)]
     -> [Double]
     -> [Codec.Picture.Image Codec.Picture.PixelRGBA8]
makeImageList2 paramHash rangeT = Prelude.map (makeFrame2 paramHash) rangeT



writeImageList2
  :: Data.Map.Map [Char] [(Double, BV Double)]
     -> [Char] -> [Double] -> [IO ()]
writeImageList2 paramHash baseFilename rangeT =
  let lenRangeT = length rangeT
      numZeros = length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
  in
    zipWith (\fname image -> Codec.Picture.writePng fname image) (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes) (makeImageList2 paramHash rangeT)

