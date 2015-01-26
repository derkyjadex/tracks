{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Train where

import Tracks.Network
import Tracks.Service
import Data.Set (Set)
import qualified Data.Set as Set

data TrainLocation = AtStation Station Line
                   | BetweenStations Station Station Line
                   deriving (Show, Ord, Eq)

data Train = Train { location :: TrainLocation
                   , service :: [Station]
                   }

data Signals = Signals { signalsNetwork :: Network
                       , occupiedSections :: Set (Line, Station, Station)
                       , occupiedStations :: Set (Line, Station)
                       }

clear :: Network -> Signals
clear network = Signals { signalsNetwork = network
                        , occupiedSections = Set.empty
                        , occupiedStations = Set.empty
                        }

startTrain :: Signals -> Line -> [Station] -> Maybe (Signals, Train)
startTrain signals@Signals { signalsNetwork, occupiedStations } line stations@(start:_)
    | Set.member (line, start) occupiedStations = Nothing
    | not $ isServiceValid signalsNetwork line stations = Nothing
    | otherwise = let train = Train { location = AtStation start line
                                    , service = drop 1 $ cycle stations
                                    }
                      occupiedStations' = Set.insert (line, start) occupiedStations
                      signals' = signals { occupiedStations = occupiedStations' }
                   in Just (signals', train)

enterStation :: Signals -> Train -> Maybe (Signals, Train)
enterStation signals@Signals { occupiedSections, occupiedStations } train@Train { location = BetweenStations prev next line, service }
    | Set.member (line, next) occupiedStations = Nothing
    | otherwise = let location' = AtStation next line
                      service' = drop 1 service
                      train' = train { location = location'
                                     , service = service'
                                     }
                      occupiedSections' = Set.delete (line, next, prev) occupiedSections
                      occupiedStations' = Set.insert (line, next) occupiedStations
                      signals' = signals { occupiedStations = occupiedStations'
                                         , occupiedSections = occupiedSections'
                                         }
                   in Just (signals', train')

enterStation _ _ = Nothing

leaveStation :: Signals -> Train -> Maybe (Signals, Train)
leaveStation signals@Signals { occupiedSections, occupiedStations } train@Train { location = AtStation station line, service }
    | Set.member (line, station, next) occupiedSections = Nothing
    | otherwise = let location' = BetweenStations station next line
                      train' = train { location = location' }
                      occupiedSections' = Set.insert (line, station, next) occupiedSections
                      occupiedStations' = Set.delete (line, station) occupiedStations
                      signals' = signals { occupiedSections = occupiedSections'
                                         , occupiedStations = occupiedStations
                                         }
                   in Just(signals', train')
    where (next:_) = service
