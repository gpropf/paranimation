module Main where

import Lib
import Params

main :: IO [()]
main = sequence $ writeImageList params "testpic" [0,0.5..8.0]
