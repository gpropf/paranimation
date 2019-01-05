module Paranimate.Modules.SierpinskyDust where

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
import Paranimate.Paranimate
import System.Random
import Number.Complex
import Algebra.Ring( C )


paramHash :: Data.Map.Map [Char] [(Double, BV Double)]
paramHash =
  fromList ([("x", [(0.0, BV (-1.5)),(50.0, BV 1.5),(500.0, BV 1.2)])
            , ("y", [(0.0, BV 0.2),(500.0,BV 0.9)])
            , ("ul", [(0.0, BVC ((-0.2) +: 1)),(500.0, BVC ((-3.0) +: 2.0))])
            , ("ll", [(0.0, BVC ((-0.2) +: (-1))),(500.0, BVC ((-5.0) +: (-2.0)))])
            , ("ur", [(0.0, BVC ((0.2) +: 1)),(500.0, BVC ((4.0) +: 4.0))])
            , ("lr", [(0.0, BVC ((0.2) +: (-1))),(500.0,BVC ((3.0) +: (-2.0)))])
            , ("t11", [(0.0, BVC ((-0.75) +: (0.5))),(500.0, BVC ((-4.75) +: (-3.5)))])
            , ("t12", [(0.0, BVC ((2) +: (2.5))),(500.0, BVC ((-2) +: (-1.5)))])
            , ("t13", [(0.0, BVC ((2.5) +: (1.25))),(500.0, BVC ((-1.5) +: (-2.75)))])
            , ("t21", [(0.0, BVC ((-2) +: (-1))),(500.0, BVC ((4) +: (5)))])
            , ("t22", [(0.0, BVC ((0.75) +: (-2.75))),(500.0, BVC ((4.75) +: (1.25)))])
            , ("t23", [(0.0, BVC ((-3) +: (-3))),(500.0, BVC ((1) +: (1)))])
            , ("vpul", [(0.0, BVC ((-4.0) +: (3.0))),(500.0, BVC ((-4.0) +: (3.0)))])
            , ("sf", [(0.0, BVC ((150.0) +: (150.0))),(500.0, BVC ((150.0) +: (150.0)))])])
            
    
makeFrame :: Data.Map.Map [Char] [(Double, BV Double)]
  -> StdGen -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame paramHash g t = do
  let white = PixelRGBA8 10 50 210 255     
      (BV x) = interpolatedValue linearInterpolate t "x" paramHash
      (BV y) = interpolatedValue linearInterpolate t "y" paramHash
      (BVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (BVC ll) = interpolatedValue linearInterpolate t "ll" paramHash
      (BVC ur) = interpolatedValue linearInterpolate t "ur" paramHash
      (BVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (BVC t11) = interpolatedValue linearInterpolate t "t11" paramHash
      (BVC t12) = interpolatedValue linearInterpolate t "t12" paramHash
      (BVC t13) = interpolatedValue linearInterpolate t "t13" paramHash
      (BVC t21) = interpolatedValue linearInterpolate t "t21" paramHash
      (BVC t22) = interpolatedValue linearInterpolate t "t22" paramHash
      (BVC t23) = interpolatedValue linearInterpolate t "t23" paramHash
      (BVC vpul) = interpolatedValue linearInterpolate t "vpul" paramHash
      (BVC sf) = interpolatedValue linearInterpolate t "sf" paramHash

      img = renderDrawing 1200 900 white $
        do          
          let vertices = [t11,t12,t13,t21,t22,t23]
              vp = Viewport { upperLeft = vec2fromComplex vpul,
                              scaleFactors = vec2fromComplex sf }
              sp = t11
              ps = pickPoints sp g numPoints vertices 0 []
          mapM_ (drawCircleCmplx vp) ps
  img



{- Module specific functions below. This is where we put graphics
 "workhorse" functions, global variables, and types for example. -}

numPoints = 250000

drawCircleCmplx vp c =
  let (n,p) = c
      ageClr = round $ ((fromInteger numPoints)-(fromInteger n))/(fromInteger numPoints) * 255
      colr = PixelRGBA8 ageClr 150 0 70
      circleR = (x $ (scaleFactors vp)) / 75.0
  in
    withTexture (uniformTexture colr) $
    fill $ circle (viewport2abs vp (Vec2 (real p) (imag p))) (realToFrac circleR)


drawCircle vp x y =
  let colr = PixelRGBA8 255 0 0 255
  in
    withTexture (uniformTexture colr) $
    fill $ circle (viewport2abs vp (Vec2 x y)) 30



{-pickPoints :: (Ord t1, RandomGen t2, Algebra.Ring.C a, Floating a, Num t1,
      Eq a) =>  T a -> t2 -> t1 -> [T a] -> [T a] -> [T a] -}
pickPoints startP g n vertices groupShift pl =
  if n <= 0 then pl else
    let numVs = (length vertices) `div` 2
        --numVs = (length vertices)
        (i,g') = randomR (0, numVs - 1) g
        (pf,g'') = randomR (1::Int,10) g'
        groupShift' = if pf < 3 then numVs else 0
        groupShiftMod = (groupShift + groupShift') `mod` (numVs*2)
        d = (0.5 `scale` ((vertices !! (i + groupShiftMod)) - startP))
--        d = (0.5 `scale` ((vertices !! i) - startP))
        newP = startP + d
    in
      pickPoints newP g' (n-1) vertices groupShiftMod ((n,newP):pl)



