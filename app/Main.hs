module Main where

import Data.List

import Control.Monad
import Lib
import Params
-- import System.Plugins.Load
import GHC
import GhcPlugins
import Language.Haskell.Interpreter


main :: IO [()]
main = sequence $ rnlm : (writeImageList params "testpic" [0,2.0..8.0])


--testghc = defaultPlugin
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
      say "Load ../src/Params.hs"
      loadModules ["../src/Params.hs", "../src/Lib.hs"]
      --oadModules ["../src/Lib.hs"]
      emptyLine
      say "Put the Prelude, Data.Map and *Params in scope"
      say "Data.Map is qualified as M!"
      setTopLevelModules ["Params","Lib"]
      setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M")]
      emptyLine
      say "Now we can query the type of an expression"
      --let params = "params"
      --say $ "e.g. typeOf " ++ params
      --say =<< typeOf params
      --emptyLine 
      --paramArg <- eval params
      -- say $ show paramArg
      runStmt "wil <- sequence $ (Lib.writeImageList params \"dyntest\" [0,2.0..8.0])"
      --say $ show writeImageListFn
      --let rng = "[0,2.0..8.0]"
      emptyLine 
