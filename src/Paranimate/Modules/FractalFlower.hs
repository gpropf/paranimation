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



paramHash :: Data.Map.Map [Char] [(Double, IV Double)]
paramHash = fromList ([("pwr", [(0.0, IV (-0.0)),(1000.0, IV 10.0)])
                      , ("ul", [(0.0, IVC ((-2.0) +: 2)),(1000.0, IVC ((1.0) +: 1.0))])
                      , ("lr", [(0.0, IVC ((2.0) +: (-2))),(1000.0, IVC ((-1.0) +: (-1.0)))])
                      , ("ll", [(0.0, IVC ((-2.0) +: (-2))),(1000.0, IVC ((-1.0) +: (1.0)))])

                      , ("accelCoeff", [(0.0,IV 0.2),(1000.0,IV 0.2)])])

{- Screen size stays constant -}
scrul = 0.0 +: 0.0
scrlr = 1600.0 +: 1200.0
scrll = 0 +: 1200.0

ul = (-2.0) +: 2.0
lr = 2.0 +: (-2.0)
ll = (-2.0) +: (-2.0)

           
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
      tracksChunked = (Data.List.map chunkTrack tracks) -- `using` parList rpar
      img = renderDrawing 1600 1200 bkgrd $
        do          
          mapM_ (drawTrackWithMatrix m) tracksChunked
    in
    img

makeStateFrame paramHash g t = do
  let m = transformationMatrix (ul,lr,ll) (scrul,scrlr,scrll)
      --(L.V3 hScale vScale zScale) = L.diagonal m
      --pst = PState m hScale vScale
      (IV accelCoeff) = interpolatedValue linearInterpolate t "accelCoeff" paramHash
      (IV pwr) = interpolatedValue linearInterpolate t "pwr" paramHash
      (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash
  putPst m 
  let greyInd = 0
      colr = PixelRGBA8 255 0 0 255
      bkgrd = PixelRGBA8 greyInd 10 greyInd 255
     

      center = Graphics.Rasterific.V2 100 100
      
      
      pmInits = (Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 600)-- `using` parList rpar
      tracks = (Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits) -- `using` parList rpar
      tracksChunked = (Data.List.map chunkTrack tracks) -- `using` parList rpar
      img = renderDrawing 1600 1200 bkgrd $
        do          
          mapM_ (drawTrackWithMatrix m) tracksChunked

        
--        withTexture (uniformTexture colr) $ do
  --          fill $ circle center 30
  
  
--  tracks <- lift $ mapM_ drawTrack tracksChunked
  return (img)



{- This compiles ...
makeStateFrame paramHash g t = do
  let greyInd = 0
      colr = PixelRGBA8 255 0 0 255
      bkgrd = PixelRGBA8 greyInd 10 greyInd 255
      (IV accelCoeff) = interpolatedValue linearInterpolate t "accelCoeff" paramHash
      (IV pwr) = interpolatedValue linearInterpolate t "pwr" paramHash
      (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
      (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
      (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash

      center = Graphics.Rasterific.V2 100 100
      m = transformationMatrix (ul,lr,ll) (scrul,scrlr,scrll)
      
      pmInits = (Data.List.map (\v -> pm { vel = v }) $ Data.List.map (vec2Scale 1) $ radialVectors 600)-- `using` parList rpar
      tracks = (Data.List.map (\pm -> track (accelerate accelCoeff pwr) (200, pm)) $ pmInits) -- `using` parList rpar
      tracksChunked = (Data.List.map chunkTrack tracks) -- `using` parList rpar
      img = renderDrawing 800 600 bkgrd $ 
        withTexture (uniformTexture colr) $ do
            fill $ circle center 30
  
  put m
--  tracks <- lift $ mapM_ drawTrack tracksChunked
  return (img)
-}

{-
makeStateFrame paramHash g t = do
   let (IVC ul) = interpolatedValue linearInterpolate t "ul" paramHash
       (IVC lr) = interpolatedValue linearInterpolate t "lr" paramHash
       (IVC ll) = interpolatedValue linearInterpolate t "ll" paramHash
       m = transformationMatrix (ul,lr,ll) (scrul,scrlr,scrll)
       testvec = Vec2 4 3
   put m
   projectPt testvec
   -}
   

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
vp = Viewport { upperLeft = Vec2 (-2) (1.5), scaleFactors = Vec2 400 400}

{- Global Helper Vars >>-}


{-<< Module helper functions -}

chunkTrack :: Integral a => [(a, b)] -> [(b, b)]
chunkTrack t = let (tEven1,tOdd1) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) $ Data.List.drop 1 t
                   (tEven2,tOdd2) = Data.List.partition (\(i,pm) -> i `mod` 2 == 0) t
               in
                 (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven1 tOdd1 )
                 ++ (Data.List.map (\((i,pmi),(j,pmj)) -> (pmi,pmj)) $ zip tEven2 tOdd2)

-- None of the types generated by ':t' in GHCI work for drawTrack
{-
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
-}
 
drawTrackWithMatrix m t = do
  let velMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . vel) p2) t
      accMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . acc) p2) t
  mapM_ (\(pmStart, pmEnd) ->
            let red = round (vec2abs (vel pmStart) / velMax * 255)
                blue = round (vec2abs (acc pmStart) / accMax * 255)
                colr = PixelRGBA8 red 0 blue 255
            in
              withTexture (uniformTexture colr) $
              stroke 3 JoinRound (CapRound, CapRound) $
              line (projectPtWithMatrix m (pos pmStart)) (projectPtWithMatrix m (pos pmEnd))) t


drawProjectedCircle p = do
  m <- get
  p' <- projectPt2 p
  let center = Graphics.Rasterific.V2 100 100
      greyInd = 0
      bkgrd = PixelRGBA8 greyInd 10 greyInd 255
      colr = PixelRGBA8 255 0 0 255
      img = renderDrawing 1600 1200 bkgrd $ 
        withTexture (uniformTexture colr) $ do
        fill $ circle p' 30
     --   drawProjectedCircle2 p'
  return img


{-
drawProjectedCircle2 :: (Control.Monad.State.MonadState
                          (L.V3 (L.V3 a1))
                          (Graphics.Rasterific.Drawing px), Real a1) => Vec2 a1 -> Control.Monad.Free.Church.F
  (Graphics.Rasterific.Drawing px) () -}

{-drawProjectedCircle2 :: (MonadState (L.V3 (L.V3 a1)) (Drawing px),
                          Real a1) =>
                        Vec2 a1 -> F (Drawing px) ()  -}
drawProjectedCircle2 p = do
  pst <- get
  p' <- projectPt2 p
  lift $ fill $ circle p' 30


  
drawTrack t = do
  pst <- get
  testP <- projectPt2 (Vec2 0.0 0.0)
  let velMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . vel) p2) t
      accMax = maximum $ Data.List.map (\(p1,p2) -> (vec2abs . acc) p2) t
      --trackLines = Data.List.map (\(pmStart, pmEnd) -> do
      trackLines = mapM_ (\(pmStart, pmEnd) -> do
                             let red = round (vec2abs (vel pmStart) / velMax * 255)
                                 blue = round (vec2abs (acc pmStart) / accMax * 255)
                                 colr = PixelRGBA8 red 0 blue 255
                                 startPt = projectPtWithMatrix (projectionMatrix pst) (pos pmStart)
                           --startPt = liftM projectPt 0.0
                                 --startPt = Graphics.Rasterific.Linear.V2 0 0
                           --startPt = projectPt (pos pmStart)
--                                 endPt = Graphics.Rasterific.Linear.V2 100 40
                                 endPt = projectPtWithMatrix (projectionMatrix pst) (pos pmEnd)
                           --sp <- startPt
                           -- ep <- enPt              
                           --startPt <- lift $ projectPt (pos pmStart)
                             withTexture (uniformTexture colr) $
                               stroke 3 JoinRound (CapRound, CapRound) $
                               line startPt endPt) t
  return (trackLines)




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
