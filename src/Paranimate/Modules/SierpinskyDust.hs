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


paramHash :: Data.Map.Map [Char] [(Double, IV Double)]
paramHash =
  fromList ([ ("s1", [(0.0, IVC ((-2) +: (-2))),(500.0, IVC ((-1) +: (-1)))])
            , ("s2", [(0.0, IVC ((-1) +: (0))),(500.0, IVC ((-3) +: (0)))])
            , ("s3", [(0.0, IVC ((-2) +: (2))),(500.0, IVC ((-1) +: (1)))])
            , ("s4", [(0.0, IVC ((0) +: (1))),(500.0, IVC ((0) +: (3)))])
            , ("s5", [(0.0, IVC ((2) +: (2))),(500.0, IVC ((1) +: (1)))])
            , ("s6", [(0.0, IVC ((1) +: (0))),(500.0, IVC ((3) +: (0)))])
            , ("s7", [(0.0, IVC ((2) +: (-2))),(500.0, IVC ((1) +: (-1)))])
            , ("s8", [(0.0, IVC ((0) +: (-1))),(500.0, IVC ((0) +: (-3)))])



            , ("x", [(0.0, IV (-1.5)),(50.0, IV 1.5),(500.0, IV 1.2)])
            , ("y", [(0.0, IV 0.2),(500.0,IV 0.9)])
            , ("ul", [(0.0, IVC ((-0.2) +: 1)),(500.0, IVC ((-3.0) +: 2.0))])
            , ("ll", [(0.0, IVC ((-0.2) +: (-1))),(500.0, IVC ((-5.0) +: (-2.0)))])
            , ("ur", [(0.0, IVC ((0.2) +: 1)),(500.0, IVC ((4.0) +: 4.0))])
            , ("lr", [(0.0, IVC ((0.2) +: (-1))),(500.0,IVC ((3.0) +: (-2.0)))])
            , ("t11", [(0.0, IVC ((-0.75) +: (0.5))),(500.0, IVC ((-4.75) +: (-3.5)))])
            , ("t12", [(0.0, IVC ((2) +: (2.5))),(500.0, IVC ((-2) +: (-1.5)))])
            , ("t13", [(0.0, IVC ((2.5) +: (1.25))),(500.0, IVC ((-1.5) +: (-2.75)))])
            , ("t21", [(0.0, IVC ((-2) +: (-1))),(500.0, IVC ((4) +: (5)))])
            , ("t22", [(0.0, IVC ((0.75) +: (-2.75))),(500.0, IVC ((4.75) +: (1.25)))])
            , ("t23", [(0.0, IVC ((-3) +: (-3))),(500.0, IVC ((1) +: (1)))])
            , ("vpul", [(0.0, IVC ((-4.0) +: (3.0))),(500.0, IVC ((-4.0) +: (3.0)))])
            , ("sf", [(0.0, IVC ((200.0) +: (200.0))),(500.0, IVC ((200.0) +: (200.0)))])])
            
    
makeFrame :: Data.Map.Map [Char] [(Double, IV Double)]
  -> StdGen -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame paramHash g t = do
  let bkg = PixelRGBA8 10 30 50 255 
      (IV x) = interpolatedValue linearInterpolate t "x" paramHash
      (IV y) = interpolatedValue linearInterpolate t "y" paramHash
      (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash
      (IVC ur) = interpolatedValue linearInterpolate t "ur" paramHash
      (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (IVC s1) = interpolatedValue linearInterpolate t "s1" paramHash
      (IVC s2) = interpolatedValue linearInterpolate t "s2" paramHash
      (IVC s3) = interpolatedValue linearInterpolate t "s3" paramHash
      (IVC s4) = interpolatedValue linearInterpolate t "s4" paramHash
      (IVC s5) = interpolatedValue linearInterpolate t "s5" paramHash
      (IVC s6) = interpolatedValue linearInterpolate t "s6" paramHash
      (IVC s7) = interpolatedValue linearInterpolate t "s7" paramHash
      (IVC s8) = interpolatedValue linearInterpolate t "s8" paramHash
      (IVC vpul) = interpolatedValue linearInterpolate t "vpul" paramHash
      (IVC sf) = interpolatedValue linearInterpolate t "sf" paramHash

      img = renderDrawing 1600 1200 bkg $
        do          
          let vertices = [s1,s2,s3,s4,s5,s6,s7,s8]
              vp = Viewport { upperLeft = vec2fromComplex vpul,
                              scaleFactors = vec2fromComplex sf }
              sp = s1
              ps = pickPoints sp g numPoints vertices 0 []
          mapM_ (drawCircleCmplx vp) ps
  img



{- Module specific functions below. This is where we put graphics
 "workhorse" functions, global variables, and types for example. -}

numPoints = 20000

drawCircleCmplx vp c =
  let (n,p) = c
      ageClr = round $ ((fromInteger numPoints)-(fromInteger n))/(fromInteger numPoints) * 255
      colr = PixelRGBA8 ageClr 150 0 100
      circleR = (x $ (scaleFactors vp)) / 125.0
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
    let numVs = (length vertices) -- `div` 2
        --numVs = (length vertices)
        (i,g') = randomR (0, numVs - 1) g
        {-
        (pf,g'') = randomR (1::Int,10) g'
        groupShift' = if pf < 3 then numVs else 0
        groupShiftMod = (groupShift + groupShift') `mod` (numVs*2) -}
        d = (0.5 `scale` ((vertices !! (i + groupShift)) - startP))
--        d = (0.5 `scale` ((vertices !! i) - startP))
        newP = startP + d
    in
      pickPoints newP g' (n-1) vertices 0 ((n,newP):pl)



