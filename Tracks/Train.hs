{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Train where

import Tracks.Network
import Tracks.Service
import Data.Set (Set)
import qualified Data.Set as Set

data TrainLocation = AtStation Station Line
                   | BetweenStations Station Station Line
                   deriving (Show, Ord, Eq)

data Train = Train { trainName :: String
                   , location :: TrainLocation
                   , service :: [Station]
                   }

instance Show Train where
        show Train { trainName, location, service } =
            trainName ++ "@" ++ show location ++ " -> " ++ show (head service)

