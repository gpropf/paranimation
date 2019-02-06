{-# LANGUAGE FlexibleContexts #-}


module Paranimate.Modules.FractalFlower where

import Prelude hiding (lookup)
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Graphics.Rasterific.Linear
--import Graphics.Rasterific.Command
--import Graphics.Rasterific.Operators
import Data.Map
import Control.Monad
import Control.Monad.State
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Maybe
import Data.List
import Number.Complex
import Algebra.Transcendental
import Graphics.Rasterific
import Paranimate.Paranimate
import System.Random
import Control.Parallel.Strategies
import Linear as L

viewportSize = 2.5

paramHash :: Data.Map.Map [Char] [(Double, IV Double)]
paramHash = fromList ([("pwr", [(0.0, IV (-0.0)),(1000.0, IV 10.0)])
                      , (prefix ++ "ul", [(0.0, IVC ((-viewportSize) +: viewportSize)),(1000.0, IVC ((-viewportSize) +: viewportSize))])
                      , (prefix ++ "lr", [(0.0, IVC ((viewportSize) +: (-viewportSize))),(1000.0, IVC ((viewportSize) +: (-viewportSize)))])
                      , (prefix ++ "ll", [(0.0, IVC ((-viewportSize) +: (-viewportSize))),(1000.0, IVC ((-viewportSize) +: (-viewportSize)))])
                     -- , (prefix ++ "geom", [(0.0, IVC (1600 +: 1200)),(1000.0, IVC (1600 +: 1200))])
                      , (prefix ++ "geom", [(0.0, IVC (800.0 +: 600.0)),(1000.0, IVC (80.0 +: 60.0))])

                      , ("accelCoeff", [(0.0,IV 0.2),(1000.0,IV 0.2)])])

{- Screen size stays constant 
scrul = 0.0 +: 0.0
scrlr = 1600.0 +: 1200.0
scrll = 0 +: 1200.0

ul = (-2.0) +: 2.0
lr = 2.0 +: (-2.0)
ll = (-2.0) +: (-2.0)
-}
{-         
makeFrame :: Data.Map.Map [Char] [(Double, IV Double)]
  -> StdGen -> Double
  -> Codec.Picture.Image Codec.Picture.PixelRGBA8
makeFrame paramHash g t = do
  let greyInd = 0
      bkgrd = PixelRGBA8 greyInd 10 greyInd 255
      (IV accelCoeff) = interpolatedValue linearInterpolate t "accelCoeff" paramHash
      (IV pwr) = interpolatedValue linearInterpolate t "pwr" paramHash
      (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash

      m = transformationMatrix (ul,lr,ll) (scrul,scrlr,scrll)
      
      pmInits = (Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 600)-- `using` parList rpar
      tracks = (Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits) -- `using` parList rpar
      --(_,tracksTransposed) = recurseState2 (tracks,[])
      tracksChunked = (Data.List.map chunkTrack tracks) -- `using` parList rpar
      img = renderDrawing 1600 1200 bkgrd $
        do          
          mapM_ (drawTrackWithMatrix m) tracksChunked
    in
    img
-}

--g = getStdGen
--t = 500::Double

makeFrame paramHash g t = do
  let greyInd = 0
      bkgrd = PixelRGBA8 greyInd 10 greyInd 255
      (IV accelCoeff) = interpolatedValue linearInterpolate t "accelCoeff" paramHash
      (IV pwr) = interpolatedValue linearInterpolate t "pwr" paramHash
      m = parametricTransform paramHash t
     {-  (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash

      m = transformationMatrix (ul,lr,ll) (scrul,scrlr,scrll)
      -}
      
      pmInits = (Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 600)-- `using` parList rpar
      tracks = (Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits) -- `using` parList rpar
      --(_,tracksTransposed) = recurseState2 (tracks,[])
      tracksTransposed = Data.List.transpose tracks
      --tracksTransposedFixed = Data.List.map fixIndices $ everyf 10 tracksTransposed
      skipn = 5
      tracksTransposedFixed = Data.List.map fixIndices $ reverse $ everyf skipn tracksTransposed
      tracksChunked = (Data.List.map chunkTrack tracksTransposedFixed) -- `using` parList rpar
      tracksChunked2 = (Data.List.map chunkTrack $ everyf skipn tracks) -- `using` parList rpar
      (IVC geom) = prefixedValue "geom"
      img = renderDrawing (round . real $ geom) (round . imag $ geom) bkgrd $
--      img = renderDrawing 800 600 bkgrd $
        do          
          mapM_ (drawTrackWithMatrix False m) tracksChunked
          mapM_ (drawTrackWithMatrix True m) tracksChunked2

    in
    img
    where
      prefixedValue valName = interpolatedValue
                              linearInterpolate t (prefix ++ valName)
                              paramHash
--    (tracks, tracksTransposed, tracksChunked)





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

{- Global Helper Vars >>-}


{-<< Module helper functions -}

fixIndices t =
  let rng = [0..(length t)]
      pms = Data.List.map (\(i,pm) -> pm) t
  in
    zip rng pms

{-
chunkTrack: breaks tracks into even and odd PMs and then constructs new lists of (even,odd) pairs. These are used as the start and end points for line segments. Just doing this, however, leads to a dashed-line effect as the points have moved between the last odd point and the next even. We thus drop a point and repeat the process. This creates new line segments from (odd,even) pairs. Finally we append the lists. This final list allows for the creation of continuous looking lines.
-}

chunkTrack :: Integral a => [(a, b)] -> [(b, b)]
chunkTrack t = let (tEven1,tOdd1) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) $ Data.List.drop 1 t
                   (tEven2,tOdd2) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) t
               in
                 (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven1 tOdd1 )
                 ++ (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven2 tOdd2)

-- None of the types generated by ':t' in GHCI work for drawTrackWithMatrix
 
drawTrackWithMatrix blueGreen m t = do
  let velMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . vel) p2) t
      accMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . acc) p2) t
  mapM_ (\(pmStart, pmEnd) ->
            let red = round (vec2abs (vel pmStart) / velMax * 255)
                blue = round (vec2abs (acc pmStart) / accMax * 255)
                colr = if blueGreen then PixelRGBA8 0 red blue 255
                       else
                         PixelRGBA8 red 0 blue 255
                         
            in
              withTexture (uniformTexture colr) $
              stroke 3 JoinRound (CapRound, CapRound) $
              line (projectPtWithMatrix m (pos pmStart)) (projectPtWithMatrix m (pos pmEnd))) t


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
