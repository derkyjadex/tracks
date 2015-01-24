{-# LANGUAGE NamedFieldPuns #-}

module Tracks where

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

data Station = Station String deriving (Show, Ord, Eq)

data Line = Line String deriving (Show, Ord, Eq)

data Section = Section Station Station deriving (Show, Ord, Eq)

data Network = Network {
             stations :: Map String Station,
             lines :: Map String Line,
             stationLines :: Map Station (Set Line),
             lineStations :: Map Line (Set Station),
             lineSections :: Map Line (Set Section)
             }

instance Show Network where
    show Network { lineStations } =
        unlines $ fmap showLine $ Map.assocs lineStations
        where showLine ((Line name), stations) =
                let numStations = Set.size stations
                 in name ++ " (" ++ (show numStations) ++ ")"


empty :: Network
empty = Network {
                stations = Map.empty,
                Tracks.lines = Map.empty,
                stationLines = Map.empty,
                lineStations = Map.empty,
                lineSections = Map.empty
                }

getStations :: Network -> [Station]
getStations Network { stations } = Map.elems stations

getLines :: Network -> [Line]
getLines Network { Tracks.lines } = Map.elems lines

getStationLines :: Network -> Station -> Maybe [Line]
getStationLines Network { stationLines } station = do
        lines <- Map.lookup station stationLines
        return $ Set.elems lines

getLineStations :: Network -> Line -> Maybe [Station]
getLineStations Network { lineStations } line = do
        stations <- Map.lookup line lineStations
        return $ Set.elems stations

getLineSections :: Network -> Line -> Maybe [Section]
getLineSections Network { lineSections } line = do
        sections <- Map.lookup line lineSections
        return $ Set.elems sections

addStation :: Network -> String -> (Station, Network)
addStation network@Network { stations, stationLines } name =
        let station = Station name
            network' = network {
                               stations = Map.insert name station stations,
                               stationLines = Map.insert station Set.empty stationLines
                               }
         in (station, network')

addLine :: Network -> String -> (Line, Network)
addLine network@Network { Tracks.lines, lineStations, lineSections } name =
        let line = Line name
            network' = network {
                               Tracks.lines = Map.insert name line lines,
                               lineStations = Map.insert line Set.empty lineStations,
                               lineSections = Map.insert line Set.empty lineSections
                               }
         in (line, network')

addSection :: Network -> Line -> Station -> Station -> Network
addSection network@Network { stationLines, lineStations, lineSections } line station1 station2 =
        let section = Section station1 station2
            stationLines' = updateSet station1 line $ updateSet station2 line stationLines
            lineStations' = updateSet line station1 $ updateSet line station2 lineStations
            lineSections' = updateSet line section lineSections
         in network {
                    stationLines = stationLines',
                    lineStations = lineStations',
                    lineSections = lineSections'
                    }
        where updateSet key value map =
                  let set = case Map.lookup key map of
                                Just set -> set
                                Nothing  -> undefined
                      set' = Set.insert value set
                   in Map.insert key set' map

addRun :: Network -> Line -> [Station] -> Network
addRun network _ [] = network
addRun network _ [_] = network
addRun network line (s1:s2:ss) =
        let network' = addSection network line s1 s2
         in addRun network' line (s2:ss)

addRunByStrings :: Network -> Line -> [String] -> Network
addRunByStrings network line names =
        let (stations, network') = mapStations ([], network) names
         in addRun network' line stations
        where mapStations result [] = result
              mapStations (prevStations, network@Network { stations }) (name:rest) =
                  let (station, network') = case Map.lookup name stations of
                                                Just station -> (station, network)
                                                Nothing      -> addStation network name
                   in mapStations ((station:prevStations), network') rest


data ReadState = ReadState Network (Maybe Line)

readTracksFile :: FilePath -> IO Network
readTracksFile path = do
        content <- readFile path
        return $ readTracksCommands content

readTracksCommands :: String -> Network
readTracksCommands content =
        let commands = filter (/="") $ Prelude.lines content
            initialState = ReadState empty Nothing
            ReadState network _ = foldl readTrackCommand initialState commands
         in network

readTrackCommand :: ReadState -> String -> ReadState
readTrackCommand (ReadState network (Just line)) ('+':run) =
        let names = splitOn "," run
            network' = addRunByStrings network line names
         in ReadState network' (Just line)

readTrackCommand (ReadState _ Nothing) ('+':_) =
        error "No Line to add run to"

readTrackCommand (ReadState network _) lineName =
        let (line, network') = addLine network lineName
         in ReadState network' (Just line)
