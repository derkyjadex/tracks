module Main where

import Tracks.Network
import Tracks.Service

getTube :: IO Network
getTube = readTracksFile "tube.tracks"

getTubeServices :: IO Services
getTubeServices = readServicesFile "tube.services"

earlsCourt, euston :: Station
earlsCourt = Station "Earl's Court"
euston = Station "Euston"

district, piccadilly, victoria, northern :: Line
district = Line "District Line"
piccadilly = Line "Piccadilly Line"
victoria = Line "Victoria Line"
northern = Line "Northern Line"

main :: IO ()
main = do
        tube <- getTube
        services <- getTubeServices
        print tube
        print services

