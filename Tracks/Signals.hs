{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Signals where

import Tracks.Network
import Tracks.Service
import Tracks.Train

import Control.Monad.STM
import Control.Concurrent
import STMContainers.Set (Set)
import qualified STMContainers.Set as Set

data Signals = Signals { signalsNetwork :: Network
                       , occupiedSections :: Set (Line, Station, Station)
                       , occupiedStations :: Set (Line, Station)
                       }

clear :: Network -> STM Signals
clear network = do
        sections <- Set.new
        stations <- Set.new
        return Signals { signalsNetwork = network
                       , occupiedSections = sections
                       , occupiedStations = stations
                       }

placeTrain :: String -> [Station] -> Line -> Signals -> STM (Maybe Train)
placeTrain name stations@(start:_) line signals@Signals{ signalsNetwork, occupiedStations } = do
        stationOccupied <- Set.lookup (line, start) occupiedStations
        if stationOccupied
            then return Nothing
            else if not $ isServiceValid stations line signalsNetwork
                     then return Nothing
                     else do
                         Set.insert (line, start) occupiedStations
                         return $ Just Train { trainName = name
                                             , location = AtStation start line
                                             , service = drop 1 $ cycle stations
                                             }

enterStation :: Train -> Signals -> STM (Maybe Train)
enterStation train@Train { location = BetweenStations prev next line, service } signals = do
        stationOccupied <- Set.lookup (line, next) $ occupiedStations signals
        if stationOccupied
            then return Nothing
            else do
                Set.insert (line, next) $ occupiedStations signals
                Set.delete (line, next, prev) $ occupiedSections signals
                return $ Just train { location = AtStation next line
                                    , service = drop 1 service
                                    }
enterStation _ _ = return Nothing

leaveStation :: Train -> Signals -> STM (Maybe Train)
leaveStation train@Train { location = AtStation station line, service } signals = do
        let (next:_) = service
        sectionOccupied <- Set.lookup (line, station, next) $ occupiedSections signals
        if sectionOccupied
            then return Nothing
            else do
                Set.insert (line, station, next) $ occupiedSections signals
                Set.delete (line, station) $ occupiedStations signals
                return $ Just train { location = BetweenStations station next line }

stepTrain :: Train -> Signals -> STM Train
stepTrain train@Train { location = AtStation _ _ } signals = do
        train' <- leaveStation train signals
        return $ case train' of
            Just t  -> t
            Nothing -> train

stepTrain train@Train { location = BetweenStations _ _ _ } signals = do
        train' <- enterStation train signals
        return $ case train' of
            Just t  -> t
            Nothing -> train

