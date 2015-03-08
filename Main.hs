module Main where

import Tracks.Network
import Tracks.Service
import Tracks.Train
import Tracks.Signals (Signals)
import qualified Tracks.Signals as Signals

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

main :: IO ()
main = do
        network <- readTracksFile "test.tracks"
        services <- readServicesFile "test.services"
        putStrLn $ writeTracksCommands network
        signals <- atomically Signals.clear
        forkIO $ startTrain "001" foxLine 1 services signals network
        forkIO $ startTrain "002" foxLine 1 services signals network
        forkIO $ startTrain "A01" newLine 1 services signals network
        getLine
        return ()

startTrain :: String -> Line -> Int -> Services -> Signals -> Network -> IO ()
startTrain name line num services signals network = do
        let service = getLineService num line services
        train <- atomically $ placeTrain name service line signals network
        case train of
            Just t  -> do
                print t
                runTrain t
            Nothing -> do
                threadDelay (500 * 1000)
                startTrain name line num services signals network

runTrain :: Train -> IO ()
runTrain train = do
        train' <- atomically $ stepTrain train
        print train'
        gen <- newStdGen
        let (delay, _) = randomR (200000, 500000) gen
        threadDelay delay
        runTrain train'
