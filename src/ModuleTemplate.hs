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
import System.Random
import Number.Complex
import Algebra.Ring( C )

testVertices = [(-1.0) +: 1, 1 +: 1, 1 +: (-1), (-1) +: (-1)]

vp :: Viewport
vp = Viewport { upperLeft = Vec2 (-2) (1.5), scaleFactors = Vec2 200 200}

paramHash :: Data.Map.Map [Char] [(Double, BV Double)]
paramHash = fromList ([("x", [(0.0, BV (-1.5)),(50.0, BV 1.5),(500.0, BV 1.2)])
                      , ("y", [(0.0, BV 0.2),(500.0,BV 0.9)])
                      , ("ul", [(0.0, BVC ((-0.2) +: 1)),(500.0, BVC ((-1.0) +: 2.0))])
                      , ("ll", [(0.0, BVC ((-0.2) +: (-1))),(500.0, BVC ((-1.0) +: (-2.0)))])
                      , ("ur", [(0.0, BVC ((0.2) +: 1)),(500.0, BVC ((1.0) +: 2.0))])
                      , ("lr", [(0.0, BVC ((0.2) +: (-1))),(500.0,BVC ((1.0) +: (-2.0)))])])

drawCircle x y =
  let colr = PixelRGBA8 255 0 0 255
  in
    withTexture (uniformTexture colr) $
    fill $ circle (viewport2abs vp (Vec2 x y)) 30


drawCircleCmplx c =
  let colr = PixelRGBA8 255 100 0 255
  in
    withTexture (uniformTexture colr) $
    fill $ circle (viewport2abs vp (Vec2 (real c) (imag c))) 3


pickPoints :: (Ord t1, RandomGen t2, Algebra.Ring.C a, Floating a, Num t1,
      Eq a) =>  T a -> t2 -> t1 -> [T a] -> [T a] -> [T a]
pickPoints startP g n vertices pl =
  if n <= 0 then pl else
    let (i,g') = randomR (0, length vertices - 1) g
        d = (0.5 `scale` ((vertices !! i) - startP))
        newP = startP + d
    in
      pickPoints newP g' (n-1) vertices (newP:pl)
    
makeFrame :: StdGen -> Data.Map.Map [Char] [(Double, BV Double)]
  -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame g paramHash t = do
  let white = PixelRGBA8 255 255 255 255     
      (BV x) = interpolatedValue linearInterpolate t "x" paramHash
      (BV y) = interpolatedValue linearInterpolate t "y" paramHash
      (BVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (BVC ll) = interpolatedValue linearInterpolate t "ll" paramHash
      (BVC ur) = interpolatedValue linearInterpolate t "ur" paramHash
      (BVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      img = renderDrawing 800 600 white $
        do          
          --return ()
          let vertices = [ul,ll,ur,lr]
              sp = (0.0 :: Double) +: (0.0 :: Double)
              ps = pickPoints sp g 100 vertices []
          mapM_ drawCircleCmplx ps
          drawCircle x y
          drawCircle (real ul) (imag ul)
    in
    img

