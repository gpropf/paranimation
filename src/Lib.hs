module Lib where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Map
import Control.Monad
import Data.Maybe
import Data.List
import Number.Complex
import Params
import Text.Printf

makeImageList2 paramHash rangeT = Prelude.map (makeFrame2 paramHash) rangeT

writeImageList2 paramHash baseFilename rangeT =
  let lenRangeT = length rangeT
      numZeros = length (show lenRangeT)
      imageIndexes = [0..lenRangeT]
      fmtString = "%0" ++ show numZeros ++ "d"
      fmt x = printf fmtString x
  in
    zipWith (\fname image -> Codec.Picture.writePng fname image) (Prelude.map (\index -> baseFilename ++ "-" ++ fmt index ++ ".png") imageIndexes) (makeImageList2 paramHash rangeT)
