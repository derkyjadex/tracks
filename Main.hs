module Main where

import Tracks.Network
import Tracks.Service

getTube :: IO Network
getTube = readTracksFile "tube.tracks"

getTubeServices :: IO Services
getTubeServices = readServicesFile "tube.services"

main :: IO ()
main = do
        tube <- getTube
        services <- getTubeServices
        print tube
        print services

