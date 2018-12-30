module Main where

import Data.List

import Control.Monad
import Paranimate
import ParamUtil
import Params
import ModuleTemplate
import System.Random
import Options.Applicative
import Data.Semigroup ((<>))

{-
data Params = Params { moduleName0 :: String,
                       range :: [Double],
                       rndSeed :: Int }
-}

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
           <> value (0,20,400)
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
    putStrLn $ "Using module " ++ m ++ " with range " ++ show rng
    case m of
      "mt" ->
        let mw = ModuleWorkers ModuleTemplate.paramHash ModuleTemplate.makeFrame
        in
          sequenceFrames mw rng "mt"
--          sequence_ $ (writeImageList (makeFrameFn mw) (pHash mw) "mt" [0,20..400.0])
--      "cpwr" -> sequence_ $ (writeImageList Params.makeFrame Params.paramHash "cpwr" [0,20..400.0])
      "cpwr" ->
        let mw = ModuleWorkers Params.paramHash Params.makeFrame
        in
          sequenceFrames mw rng "cpwr"
      _ -> putStrLn $ "Module " ++ m ++ " not found!"
    
runModule _ = return ()


sequenceFrames mw (start,step,end) baseFilename =  
  sequence_ $ (writeImageList (makeFrameFn mw) (pHash mw) baseFilename [start,start+step..end])
{-
main :: IO [()]
main = sequence $ (writeImageList ModuleTemplate.makeFrame ModuleTemplate.paramHash "paranimate" [0,5..400.0])

getRandomNum :: (Random a, Show a, Show g, RandomGen g, Fractional a) => g -> a -> a -> String
getRandomNum g a b = show $ randomR (a, b) g
-- FIXME

-}



--main = sequence [rnlm]
{-

errorString :: InterpreterError -> String
errorString (WontCompile es) = intercalate "\n" (header : map unbox es)
  where
    header = "ERROR: Won't compile:"
    unbox (GhcError e) = e
errorString e = show e

say :: String -> Interpreter ()
say = liftIO . putStrLn

emptyLine :: Interpreter ()
emptyLine = say ""



rnlm = do r <- runInterpreter testHint
          case r of
            Left err -> putStrLn $ errorString err
            Right () -> return ()


testHint =
    do
      say "Load ../src/Params.hs, ../src/ParamUtil.hs, ../src/Paranimate.hs"
      -- loadModules ["../src/Params.hs", "../src/ParamUtil.hs", "../src/Paranimate.hs"]
      -- loadModules ["Params", "ParamUtil", "Paranimate"]
      loadModules ["Params"]      
      setTopLevelModules ["Params"]
     -- setImportsQ [(ParamUtil, Just "P")]
      runStmt "wil <- sequence $ (ParamUtil.writeImageList2 paramHash \"dyntest\" [1.0,2.0..9.0])"
      emptyLine 
-}
