{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Network (
      Station(..)
    , Line(..)
    , Section(..)

    , Network
    , empty
    , getStations
    , getLines
    , getStationLines
    , getLineStations
    , getLineSections
    , getAdjacentStations
    , getAdjacentStationsOnLine

    , addStation
    , addLine
    , addSection
    , addRun
    , addRunByStrings

    , readTracksFile
    , readTracksCommands
    , writeTracksCommands
    ) where

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Station = Station String deriving (Show, Ord, Eq)

data Line = Line String deriving (Show, Ord, Eq)

data Section = Section Station Station deriving (Show, Ord, Eq)

data Network = Network { stations :: Map String Station
                       , trackLines :: Map String Line
                       , stationLines :: Map Station (Set Line)
                       , lineStations :: Map Line (Set Station)
                       , lineSections :: Map Line (Set Section)
                       }

instance Show Network where
    show Network { lineStations } =
        unlines $ fmap showLine $ Map.assocs lineStations
        where showLine ((Line name), stations) =
                let numStations = Set.size stations
                 in name ++ " (" ++ (show numStations) ++ ")"


empty :: Network
empty = Network { stations = Map.empty
                , trackLines = Map.empty
                , stationLines = Map.empty
                , lineStations = Map.empty
                , lineSections = Map.empty
                }

getStations :: Network -> [Station]
getStations Network { stations } = Map.elems stations

getLines :: Network -> [Line]
getLines Network { trackLines } = Map.elems trackLines

getStationLines :: Network -> Station -> [Line]
getStationLines Network { stationLines } station =
        case Map.lookup station stationLines of
            Just lines -> Set.elems lines
            Nothing    -> []

getLineStations :: Network -> Line -> [Station]
getLineStations Network { lineStations } line =
        case Map.lookup line lineStations of
            Just stations -> Set.elems stations
            Nothing       -> []

getLineSections :: Network -> Line -> [Section]
getLineSections Network { lineSections } line =
        case Map.lookup line lineSections of
            Just sections -> Set.elems sections
            Nothing       -> []

getAdjacentStationsOnLine :: Network -> Station -> Line -> [Station]
getAdjacentStationsOnLine network station line =
        let sections = filter hasStation $ getLineSections network line
         in fmap otherStation sections
        where hasStation (Section a b) = a == station || b == station
              otherStation (Section a b) =
                if a == station then b else a

getAdjacentStations :: Network -> Station -> [(Line, Station)]
getAdjacentStations network station =
        let lines = getStationLines network station
         in concatMap lineRoutes lines
        where lineRoutes line = zip (repeat line) $ getStations line
              getStations = getAdjacentStationsOnLine network station

addStation :: Network -> String -> (Station, Network)
addStation network@Network { stations, stationLines } name =
        let station = Station name
            network' = network {
                               stations = Map.insert name station stations,
                               stationLines = Map.insert station Set.empty stationLines
                               }
         in (station, network')

addLine :: Network -> String -> (Line, Network)
addLine network@Network { trackLines, lineStations, lineSections } name =
        let line = Line name
            network' = network {
                               trackLines = Map.insert name line trackLines,
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

writeTracksFile :: FilePath -> Network -> IO ()
writeTracksFile path network =
        writeFile path $ writeTracksCommands network

writeTracksCommands :: Network -> String
writeTracksCommands network =
        unlines $ concatMap writeLine $ getLines network
        where writeLine line@(Line lineName) =
                lineName : (writeLineSections $ getLineSections network line)
              writeLineSections ((Section (Station a) (Station b)):rest) =
                  ("+" ++ a ++ "," ++ b) : writeLineSections rest
              writeLineSections [] = [""]

