module Main where

import Tracks.Network
import Tracks.Service
import Tracks.Train (Train)
import Tracks.Signals

import Control.Monad.STM
import Control.Concurrent
import qualified STMContainers.Set as Set
import System.Random

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

main :: IO ()
main = do
        putStrLn $ writeTracksCommands network
        signals <- atomically $ clear network
        forkIO $ startTrain "001" foxLine 1 services signals
        forkIO $ startTrain "002" foxLine 1 services signals
        forkIO $ startTrain "A01" newLine 1 services signals
        getLine
        return ()

startTrain :: String -> Line -> Int -> Services -> Signals -> IO ()
startTrain name line num services signals = do
        let service = getLineService num line services
        train <- atomically $ placeTrain name service line signals
        case train of
            Just t  -> do
                print t
                runTrain t signals
            Nothing -> do
                threadDelay (500 * 1000)
                startTrain name line num services signals

runTrain :: Train -> Signals -> IO ()
runTrain train signals = do
        train' <- atomically $ stepTrain train signals
        print train'
        gen <- newStdGen
        let (delay, _) = randomR (200000, 500000) gen
        threadDelay delay
        runTrain train' signals
