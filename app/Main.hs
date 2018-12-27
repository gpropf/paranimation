module Main where

import Data.List

import Control.Monad
import Paranimate
import ParamUtil
import Params
-- import System.Plugins.Load
--import GHC
--import GhcPlugins
--import Language.Haskell.Interpreter

{-
Right now this only works in GHCI running the ./src directory. The executable
doesn't work because it can't find the graphics modules.
-}


main :: IO [()]
-- main = sequence $ rnlm : (writeImageList params "testpic" [0,2.0..8.0])
main = sequence $ (writeImageList paramHash "paranimate" [0,1.0..499.0])
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
