{-# LANGUAGE FlexibleContexts, DeriveGeneric, FlexibleInstances #-}

module Paranimate.Deprecated where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear as GL
import Graphics.Rasterific.Transformations as GT
--import Graphics.Rasterific.Command
import Data.Map
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.List
import Number.Complex
import Algebra.Ring( C )
import Text.Printf
import System.Random
import Control.Parallel.Strategies
import Linear as L
import Control.Monad.Free.Church( F, fromF )
import GHC.Generics
import Data.Aeson

import Paranimate.Paranimate


{- Viewport: A simple way to define the virtual space occupied by the frame. Since we currently use the Rasterific graphics framework the drawing primitives use pixel coordinates. For most animations, though, there is a need to use an abstract cartesian coordinate system centered on 0,0. A good example is the Mandelbrot set. Almost everything interesting this classic fractal happens in the square defined by -2,-2 and 2,2. We need a simple way to map coordinates like this to pixel-space. This type does that. As an example, let's say we wanted to draw a Mandelbrot set on a 1000x800 pixel canvas with the boundaries as described above. Note that the frame is 4 units wide and 4 tall in the Mandelbrot space, the 0,0 point in the pixel plane is analagous to the -2,-2 point in the Mandelbrot space, and the 1000,800 point in pixel space is at 2,2 in Mandelbrot space. This means that upperLeft is -2,-2 and scaleFactors is (1000 / 4),(800 / 4). Zoom and pan effects can be created by altering upperLeft and/or scaleFactors as needed. -}

data Viewport = Viewport { upperLeft :: Vec2 Double, scaleFactors :: Vec2 Double} deriving (Show)

{- Deprecated but needed for SierpinskyDust module until I rewrite it -} 
viewport2abs :: Fractional a => Viewport -> Vec2 Double -> GL.V2 a
viewport2abs vp p =
  let px  = (x p - (x $ upperLeft vp)) * (x $ scaleFactors vp)
      py  = ((y $ upperLeft vp) - y p) * (y $ scaleFactors vp)
  in
    vec2toV2 $ Vec2 px py

