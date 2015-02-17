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
            (_, b) = addLine "Fox" a
            (_, c) = addLine "New" b
            d = addRun [foxhole, riverford, tunwall] foxLine c
            e = addRun [maccton, riverford, welbridge] newLine d
         in e

services :: Services
services =
        let a = Tracks.Service.empty
            (_, b) = addService [foxhole, riverford, tunwall, riverford] foxLine a
            (_, c) = addService [maccton, riverford, welbridge, riverford] newLine b
         in c

signals :: Signals
signals = clear network

startService :: Services -> Signals -> Line -> Int -> Maybe (Signals, Train)
startService services signals line num =
        let stations = getLineService num line services
         in startTrain signals line stations

main :: IO ()
main = do
        print network
        print foxLine
        print newLine
        let service = getLineService 1 foxLine services
        print $ isServiceValid service foxLine network
        case startService services signals foxLine 1 of
            Just result -> do
                _ <- runTrain result
                return ()
            Nothing     -> print ":("

main2 :: IO ()
main2 = do
        network <- readTracksFile "tube.tracks"
        services <- readServicesFile "tube.services"
        let victoria = Line "Victoria Line"
            service = getLineService 1 victoria services
            signals = clear network
        print $ isServiceValid service victoria network
        case startService services signals victoria 1 of
            Just result -> do
                _ <- runTrain result
                return ()
            Nothing     -> print ":("

runTrain :: (Signals, Train) -> IO (Signals, Train)
runTrain (signals, train) = do
        print train
        let (signals', train') = stepTrain signals train
        threadDelay $ 300 * 1000
        runTrain (signals', train')
