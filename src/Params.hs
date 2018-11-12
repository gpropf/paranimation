module Params where


import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
--import Data.Map
import Data.Map
--import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import Number.Complex
import Util
import Algebra.Transcendental
import Graphics.Rasterific

params :: Map String [(Double, [Double])]
params = fromList ([("r", [(0.5, [5]), (10.0, [ 20, 20 ]), (20.0, [ 60, 5, 30])])
                   , ("x", [(0.0, [1,2,3]), (1.0, [ 10, 20, 30]), (2.0, [ 100, 200, 300])])
                   , ("vx", [(0.5, [0.1,0.2]), (100.0, [ (-0.1), (0.2)])])
                   , ("vy", [(0.5, [0.1,0.0]), (10.0, [ (0.1), (0.2)])])
                   , ("y", [(0.0, [ 1, 2, 3]), (1.0, [ 10, 20, 30]), (2.0, [ 101, 201, 301])])
                   , ("accelCoeff", [(100.0, [ 0, 0.2])])
                   , ("pwr", [(100.0, [ 0, 1.0])])])


-- Utility functions for complex square acceleration movies

  
z = Vec2 0 0
pm = PointMass 4 z (Vec2 0.1 0.1) (Vec2 (-0.01) 0.02)


chunkTrack :: Integral a => [(a, b)] -> [(b, b)]
chunkTrack t = let (tEven1,tOdd1) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) $ Data.List.drop 1 t
                   (tEven2,tOdd2) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) t
               in
                 -- zip (Data.List.map (\(i,pm) -> pm) tEven) (Data.List.map (\(i,pm) -> tOdd))
                 (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven1 tOdd1 )
                 ++ (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven2 tOdd2)



vp = Viewport { upperLeft = Vec2 (-2) (1.5), scaleFactors = Vec2 200 200}


drawTrack t = do
  let velMax = maximum $ Data.List.map (\(p1,p2) -> (v2abs . vel) p2) t
      accMax = maximum $ Data.List.map (\(p1,p2) -> (v2abs . acc) p2) t
  mapM_ (\(pmStart, pmEnd) ->
            let red = round (v2abs (vel pmStart) / velMax * 255)
                blue = round (v2abs (acc pmStart) / accMax * 255)
                --  ((v2abs (acc pmStart) * 200) + (v2abs (acc pmEnd) * 200))/2)
                colr = PixelRGBA8 red 0 blue 255
            in
              withTexture (uniformTexture colr) $
              stroke 3 JoinRound (CapRound, CapRound) $
              line (viewport2abs vp (pos pmStart)) (viewport2abs vp (pos pmEnd))) t
            




makeFrame2 :: Map String (Maybe Double) -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame2 params = do
  let white = PixelRGBA8 255 255 255 255
      drawColor = PixelRGBA8 0 0x86 0xc1 255
      vx = fromJust $ fromJust $ Data.Map.lookup "vx" params
      vy = fromJust $ fromJust $ Data.Map.lookup "vy" params
      r = fromJust $ fromJust $ Data.Map.lookup "r" params
      accelCoeff = fromJust $ fromJust $ Data.Map.lookup "accelCoeff" params
      pwr = fromJust $ fromJust $ Data.Map.lookup "pwr" params
      {- pm' = pm { vel = Vec2 vx vy }
      pms = track accelerate (20,pm')
      zpms = chunkTrack pms -}

      pmInits = Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 60
      tracks = Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits
      tracksChunked = Data.List.map chunkTrack tracks
      img = renderDrawing 800 600 white $
        withTexture (uniformTexture drawColor) $
        do
          -- fill $ circle (V2 0 0) 30
          -- stroke 4 JoinRound (CapRound, CapRound) $
            -- circle (V2 400 200) (40 + realToFrac r)
          --drawTrack zpms
          mapM_ drawTrack tracksChunked
    in
    img


data PointMass = PointMass { mass :: Double,
                             pos :: Vec2 Double,
                             vel :: Vec2 Double,
                             acc :: Vec2 Double
                           } deriving Show


accelerate :: Double -> Double -> PointMass -> PointMass
accelerate accelCoeff pwr pm =
  let pmPos = pos pm
      complexPos = (x pmPos) +: (y pmPos)
      pmVel = vel pm
      pmVelMag = v2abs pmVel
      t = if pmVelMag > 0.01 then 0.01 / pmVelMag else 1
  in
    pm { pos = (pos pm |+ (vec2Scale t $ vel pm))
       , vel = (vel pm |+ acc pm)
       , acc = complex2Vec2 $ scale accelCoeff $ power (realToFrac pwr) complexPos}

track :: (Ord a, Num a) => (t -> t) -> (a, t) -> [(a, t)]
track f pmInit = unfoldr (\(numSteps, pm) -> if numSteps <= 0 then Nothing else Just ((numSteps,pm),(numSteps - 1, f pm))) pmInit

radialVectors :: (Algebra.Transcendental.C a, Floating a, Enum a) => a -> [Vec2 a]
radialVectors n =
  let rng = [0..(n-1)]
      angle = 2 * Prelude.pi / n
  in
    --Data.List.map (\c -> Vec2 (real c) (imag c))
    Data.List.map ((\c -> Vec2 (real c) (imag c)) . cis . (* angle)) rng
