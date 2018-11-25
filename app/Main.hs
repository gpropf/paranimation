module Main where

import Lib
import Params
-- import System.Plugins.Load

main :: IO [()]
main = sequence $ writeImageList params "testpic" [0,0.5..8.0]
