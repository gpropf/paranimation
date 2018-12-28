module Main where

import Data.List

import Control.Monad
import Paranimate
import ParamUtil
import Params
import ModuleTemplate
import System.Random

main :: IO [()]
main = sequence $ (writeImageList ModuleTemplate.makeFrame ModuleTemplate.paramHash "paranimate" [0,5..400.0])

getRandomNum :: (Random a, Show a, Show g, RandomGen g, Fractional a) => g -> a -> a -> String
getRandomNum g a b = show $ randomR (a, b) g
-- FIXME




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
