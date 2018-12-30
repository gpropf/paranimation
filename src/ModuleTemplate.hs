module ModuleTemplate where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.Map
import Control.Monad
import Data.Maybe
import Data.List
import Number.Complex
import Algebra.Transcendental
import Graphics.Rasterific
import Paranimate


vp :: Viewport
vp = Viewport { upperLeft = Vec2 (-2) (1.5), scaleFactors = Vec2 200 200}

paramHash :: Data.Map.Map [Char] [(Double, BV Double)]
paramHash = fromList ([("x", [(0.0, BV (-1.5)),(50.0, BV 1.5),(500.0, BV 1.2)])
                      , ("y", [(0.0, BV 0.2),(500.0,BV 0.9)])
                      , ("ul", [(0.0, BVC ((-0.2) +: 1)),(500.0,BVC ((-1.0) +: 2.0))])])

drawCircle x y =
  let colr = PixelRGBA8 255 0 0 255
  in
    withTexture (uniformTexture colr) $
    fill $ circle (viewport2abs vp (Vec2 x y)) 30

    
makeFrame :: Data.Map.Map [Char] [(Double, BV Double)]
  -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame paramHash t = do
  let white = PixelRGBA8 255 255 255 255     
      (BV x) = interpolatedValue linearInterpolate t "x" paramHash
      (BV y) = interpolatedValue linearInterpolate t "y" paramHash
      (BVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      img = renderDrawing 800 600 white $
        do          
          --return ()
          drawCircle x y
          drawCircle (real ul) (imag ul)
    in
    img

