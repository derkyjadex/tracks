module Main where

import Tracks.Network
import Tracks.Service
import Tracks.Train
import Control.Concurrent (threadDelay)

foxhole, riverford, tunwall, maccton, welbridge :: Station
foxhole = Station "Foxhole"
riverford = Station "Riverford"
tunwall = Station "Tunwall"
maccton = Station "Maccton"
welbridge = Station "Welbridge"

foxLine, newLine :: Line
foxLine = Line "Fox"
newLine = Line "New"

network :: Network
network =
        let a = Tracks.Network.empty
            (_, b) = addLine a "Fox"
            (_, c) = addLine b "New"
            d = addRun c foxLine [foxhole, riverford, tunwall]
            e = addRun d newLine [maccton, riverford, welbridge]
         in e

services :: Services
services =
        let a = Tracks.Service.empty
            (_, b) = addService a foxLine [foxhole, riverford, tunwall, riverford]
            (_, c) = addService b newLine [maccton, riverford, welbridge, riverford]
         in c

signals :: Signals
signals = clear network

startService :: Services -> Signals -> Line -> Int -> Maybe (Signals, Train)
startService services signals line id =
        let stations = getLineService services line id
         in startTrain signals line stations

main :: IO ()
main = do
        print network
        print foxLine
        print newLine
        let service = getLineService services foxLine 1
        print $ isServiceValid network foxLine service
        case startService services signals foxLine 1 of
            Just result -> do
                runTrain result
                return ()
            Nothing     -> print ":("

main2 :: IO ()
main2 = do
        network <- readTracksFile "tube.tracks"
        services <- readServicesFile "tube.services"
        let victoria = Line "Victoria Line"
            service = getLineService services victoria 1
            signals = clear network
        print $ isServiceValid network victoria service
        case startService services signals victoria 1 of
            Just result -> do
                runTrain result
                return ()
            Nothing     -> print ":("

runTrain :: (Signals, Train) -> IO (Signals, Train)
runTrain (signals, train) = do
        print train
        let (signals', train') = stepTrain signals train
        threadDelay $ 300 * 1000
        runTrain (signals', train')
