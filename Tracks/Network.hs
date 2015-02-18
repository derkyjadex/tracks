{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Tracks.Network (
      Station(..)
    , Line(..)
    , Section(..)

    , Network
    , Tracks.Network.empty
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
    , writeTracksFile
    , writeTracksCommands
    ) where

import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Hashable
import GHC.Generics

data Station = Station String deriving (Show, Ord, Eq, Generic)

instance Hashable Station

data Line = Line String deriving (Show, Ord, Eq, Generic)

instance Hashable Line

data Section = Section Station Station deriving (Show, Ord, Eq, Generic)

instance Hashable Section

data Network = Network { stations :: Map String Station
                       , trackLines :: Map String Line
                       , stationLines :: Map Station (Set Line)
                       , lineStations :: Map Line (Set Station)
                       , lineSections :: Map Line (Set Section)
                       }

instance Show Network where
    show Network { lineStations } =
        unlines $ showLine <$> Map.assocs lineStations
        where showLine (Line name, stations) =
                let numStations = Set.size stations
                 in name ++ " (" ++ show numStations ++ ")"


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

getStationLines :: Station -> Network -> [Line]
getStationLines station Network { stationLines } =
        case Map.lookup station stationLines of
            Just ls -> Set.elems ls
            Nothing -> []

getLineStations :: Line -> Network -> [Station]
getLineStations line Network { lineStations } =
        case Map.lookup line lineStations of
            Just stations -> Set.elems stations
            Nothing       -> []

getLineSections :: Line -> Network -> [Section]
getLineSections line Network { lineSections } =
        case Map.lookup line lineSections of
            Just station -> Set.elems station
            Nothing      -> []

getAdjacentStationsOnLine :: Station -> Line -> Network -> [Station]
getAdjacentStationsOnLine station line network =
        let sections = filter hasStation $ getLineSections line network
         in fmap otherStation sections
        where hasStation (Section a b) = a == station || b == station
              otherStation (Section a b) =
                if a == station then b else a

getAdjacentStations :: Station -> Network -> [(Line, Station)]
getAdjacentStations station network =
        let ls = getStationLines station network
         in concatMap lineRoutes ls
        where lineRoutes line =
                zip (repeat line) $ getAdjacentStationsOnLine station line network

addStation :: String -> Network -> (Station, Network)
addStation name network@Network { stations, stationLines } =
        let station = Station name
            network' = network { stations = Map.insert name station stations
                               , stationLines = Map.insert station Set.empty stationLines
                               }
         in (station, network')

addLine :: String -> Network -> (Line, Network)
addLine name network@Network { trackLines, lineStations, lineSections } =
        let line = Line name
            network' = network { trackLines = Map.insert name line trackLines
                               , lineStations = Map.insert line Set.empty lineStations
                               , lineSections = Map.insert line Set.empty lineSections
                               }
         in (line, network')

addSection :: Station -> Station -> Line -> Network -> Network
addSection s1 s2 line network@Network { stationLines, lineStations, lineSections } =
        let section = Section s1 s2
            stationLines' = updateSet s1 line $ updateSet s2 line stationLines
            lineStations' = updateSet line s1 $ updateSet line s2 lineStations
            lineSections' = updateSet line section lineSections
         in network { stationLines = stationLines'
                    , lineStations = lineStations'
                    , lineSections = lineSections'
                    }
        where updateSet k v m =
                  let set = fromMaybe undefined (Map.lookup k m)
                      set' = Set.insert v set
                   in Map.insert k set' m

addRun :: [Station] -> Line -> Network -> Network
addRun [] _ network = network
addRun [_] _ network = network
addRun (s1:s2:ss) line network =
        let network' = addSection s1 s2 line network
         in addRun (s2:ss) line network'

addRunByStrings :: [String] -> Line -> Network -> Network
addRunByStrings names line network =
        let (stations, network') = mapStations ([], network) names
         in addRun stations line network'
      where mapStations result [] = result
            mapStations (prevStations, n@Network { stations }) (name:rest) =
                let (station, network') = case Map.lookup name stations of
                                              Just s  -> (s, network)
                                              Nothing -> addStation name n
                 in mapStations (station:prevStations, network') rest


data ReadState = ReadState Network (Maybe Line)

readTracksFile :: FilePath -> IO Network
readTracksFile path = do
        content <- readFile path
        return $ readTracksCommands content

readTracksCommands :: String -> Network
readTracksCommands content =
        let commands = filter (/="") $ Prelude.lines content
            initialState = ReadState Tracks.Network.empty Nothing
            ReadState network _ = foldl readTrackCommand initialState commands
         in network

readTrackCommand :: ReadState -> String -> ReadState
readTrackCommand (ReadState network (Just line)) ('+':run) =
        let names = splitOn "," run
            network' = addRunByStrings names line network
         in ReadState network' (Just line)

readTrackCommand (ReadState _ Nothing) ('+':_) =
        error "No Line to add run to"

readTrackCommand (ReadState network _) lineName =
        let (line, network') = addLine lineName network
         in ReadState network' (Just line)

writeTracksFile :: FilePath -> Network -> IO ()
writeTracksFile path network =
        writeFile path $ writeTracksCommands network

writeTracksCommands :: Network -> String
writeTracksCommands network =
        unlines $ concatMap writeLine $ getLines network
        where writeLine line@(Line lineName) =
                lineName : writeLineSections (getLineSections line network)
              writeLineSections (Section (Station a) (Station b) : rest) =
                  ("+" ++ a ++ "," ++ b) : writeLineSections rest
              writeLineSections [] = [""]

