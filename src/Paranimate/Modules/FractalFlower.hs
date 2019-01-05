module Paranimate.Modules.FractalFlower where

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

paramHash :: Data.Map.Map [Char] [(Double, BV Double)]
paramHash = fromList ([("pwr", [(0.0, BV 2.0),(50.0, BV 6.0),(500.0, BV 7.0)])
                      , ("accelCoeff", [(0.0,BV 0.2),(500.0,BV 0.2)])])


           
makeFrame :: Data.Map.Map [Char] [(Double, BV Double)]
  -> StdGen -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame paramHash g t = do
  let white = PixelRGBA8 255 255 255 255
      (BV accelCoeff) = interpolatedValue linearInterpolate t "accelCoeff" paramHash
      (BV pwr) = interpolatedValue linearInterpolate t "pwr" paramHash

      pmInits = Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 60
      tracks = Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits
      tracksChunked = Data.List.map chunkTrack tracks
      img = renderDrawing 800 600 white $
        do          
          mapM_ drawTrack tracksChunked
    in
    img



{- Module specific functions below. This is where we put graphics
 "workhorse" functions, global variables, and types for example. -}


data PointMass = PointMass { mass :: Double,
                             pos :: Vec2 Double,
                             vel :: Vec2 Double,
                             acc :: Vec2 Double
                           } deriving Show

{-<< Global Helper Vars -}

z :: Vec2 Double
z = Vec2 0 0

pm :: PointMass
pm = PointMass 4 z (Vec2 0.1 0.1) (Vec2 (-0.01) 0.02)

vp :: Viewport
vp = Viewport { upperLeft = Vec2 (-2) (1.5), scaleFactors = Vec2 200 200}

{- Global Helper Vars >>-}


{-<< Module helper functions -}

chunkTrack :: Integral a => [(a, b)] -> [(b, b)]
chunkTrack t = let (tEven1,tOdd1) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) $ Data.List.drop 1 t
                   (tEven2,tOdd2) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) t
               in
                 (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven1 tOdd1 )
                 ++ (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven2 tOdd2)

-- None of the types generated by ':t' in GHCI work for drawTrack
drawTrack t = do
  let velMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . vel) p2) t
      accMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . acc) p2) t
  mapM_ (\(pmStart, pmEnd) ->
            let red = round (vec2abs (vel pmStart) / velMax * 255)
                blue = round (vec2abs (acc pmStart) / accMax * 255)
                colr = PixelRGBA8 red 0 blue 255
            in
              withTexture (uniformTexture colr) $
              stroke 3 JoinRound (CapRound, CapRound) $
              line (viewport2abs vp (pos pmStart)) (viewport2abs vp (pos pmEnd))) t


accelerate :: Double -> Double -> PointMass -> PointMass
accelerate accelCoeff pwr pm =
  let pmPos = pos pm
      complexPos = (x pmPos) +: (y pmPos)
      pmVel = vel pm
      pmVelMag = vec2abs pmVel
      t = if pmVelMag > 0.01 then 0.01 / pmVelMag else 1
  in
    pm { pos = (pos pm |+ (vec2Scale t $ vel pm))
       , vel = (vel pm |+ acc pm)
       , acc = vec2fromComplex $ scale accelCoeff $ power (realToFrac pwr) complexPos}


track :: (Ord a, Num a) => (t -> t) -> (a, t) -> [(a, t)]
track f pmInit = unfoldr (\(numSteps, pm) -> if numSteps <= 0 then Nothing else Just ((numSteps,pm),(numSteps - 1, f pm))) pmInit


radialVectors :: (Algebra.Transcendental.C a, Floating a, Enum a) => a -> [Vec2 a]
radialVectors n =
  let rng = [0..(n-1)]
      angle = 2 * Prelude.pi / n
  in
    Data.List.map ((\c -> Vec2 (real c) (imag c)) . cis . (* angle)) rng

{- Module helper functions >>-}
