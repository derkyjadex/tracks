module Main where

import Tracks.Network

getTube :: IO Network
getTube = readTracksFile "tube.tracks"

main :: IO ()
main = do
        tube <- getTube
        print tube

