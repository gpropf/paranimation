module Main where

import Data.List

import Control.Monad
import Paranimate.Paranimate
-- import Paranimate.ParamUtil
import Paranimate.Modules.FractalFlower as FF
import Paranimate.Modules.SierpinskyDust as SD
import System.Random
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Parallel.Strategies
import Text.Printf
import Codec.Picture( Image, PixelRGBA8( .. ), writePng )

data Params = Params
  { moduleName :: String
  , quiet      :: Bool
  , range :: (Double, Double, Double) }


params :: Parser Params
params = Params
         <$> strOption
         ( long "module-name"
           <> short 'm'
           <> metavar "MODULENAME"
           <> help "Animation module to use." )
         <*> switch
         ( long "quiet"
           <> short 'q'
           <> help "Whether to be quiet" )
         <*> option auto
         ( long "paramRange"
           <> short 'r'
           <> help "Range of values for independent variable: (start, step, end)"
           <> showDefault
           <> value (0,20,489)
           <> metavar "RANGE" )


main :: IO ()
main = runModule =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "Generate a sequence of frames using module MODULENAME"
     <> header "paranimate - generate animations based on changing parameters." )

runModule :: Params -> IO ()
runModule (Params m False rng) =
  do
    g <- getStdGen
    putStrLn $ "Using module " ++ m ++ " with range " ++ show rng
    case m of
      "sd" ->
        let mw = ModuleWorkers SD.paramHash SD.makeFrame
        in
          sequenceFrames mw g rng "sd"
      "ff" ->
        let mw = ModuleWorkers FF.paramHash FF.makeFrame
        in
          sequenceFrames mw g rng "ff"
      _ -> putStrLn $ "Module " ++ m ++ " not found!"
    
runModule _ = return ()

makeGs n g gs =
  if n <= 0 then gs else
    let (g1, g2) = split g
    in
      makeGs (n-1) g2 (g1:gs)

makeParFrame mw baseFilename g i t =
  let fmtString = "%03d"
      fmt x = printf fmtString x
      fname = baseFilename ++ "-" ++ fmt i ++ ".png"
  in
    Codec.Picture.writePng fname $ (makeFrameFn mw) (pHash mw) g t
    
sequenceFrames
  :: ModuleWorkers -> StdGen -> (Double, Double, Double) -> [Char] -> IO ()
sequenceFrames mw g (start,step,end) baseFilename =
  do
    let rng = [start,start+step..end]
        gs = makeGs (length rng) g []
    sequence_ $ (writeImageList (makeFrameFn mw) (pHash mw) baseFilename gs rng)
{-
        frameActions = writeImageList (makeFrameFn mw) (pHash mw) baseFilename gs rng
--    putStrLn "FOOO"
        frameActions' = frameActions `using` parList rpar
    mapM_ (\x -> do x) frameActions'
-}

{-
zipTest mw baseFilename g =
  let rngI = [(0::Int)..10]
      rngT = [(0::Double)..10]
  in
    zipWith
    (\i t -> makeParFrame mw baseFilename g i t)
    rngI
    rngT

  -}  
{-
 ior <- (writeImageList (makeFrameFn mw) (pHash mw) baseFilename gs rng)
    let ior' = ior `using` parList rseq
    mapM_ (\x -> return x) ior'


----------------

    imgActions <- (writeImageList (makeFrameFn mw) (pHash mw) baseFilename gs rng)
    imgActions `usingIO` parList rpar
    --mapM_ (\x -> return x) imgActions'
    --imgActions'
-}
