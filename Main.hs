module Main where

import Tracks

getTube :: IO Network
getTube = readTracksFile "tube.tracks"

main :: IO ()
main = do
        tube <- getTube
        print tube

