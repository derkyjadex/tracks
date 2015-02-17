{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Service (
      Services
    , Tracks.Service.empty
    , getLineServices
    , getLineService

    , invalidSteps
    , isServiceValid

    , addService

    , readServicesFile
    , readServicesCommands
    ) where

import Tracks.Network
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Applicative

data Services = Services (Map Line (Map Int [Station]))

instance Show Services where
        show (Services ls) =
            unlines $ showLine <$> Map.assocs ls
            where showLine (Line name, services) =
                    let servicesInfo = intercalate ", " $ showService <$> Map.assocs services
                     in name ++ " (" ++ servicesInfo ++ ")"
                  showService (num, stations) =
                      show num ++ "-" ++ show (length stations)


empty :: Services
empty = Services Map.empty

getLineServices :: Line -> Services -> [Int]
getLineServices line (Services ls) =
        case Map.lookup line ls of
            Just services -> Map.keys services
            Nothing     -> []

getLineService :: Int -> Line -> Services -> [Station]
getLineService num line (Services ls) =
        case Map.lookup line ls of
            Just services -> fromMaybe [] (Map.lookup num services)
            Nothing       -> []

addService :: [Station] -> Line -> Services -> (Int, Services)
addService service line (Services ls) =
        let (services, num) = case Map.lookup line ls of
                               Just ss -> let (n, _) = Map.findMax ss
                                          in (ss, n)
                               Nothing     -> (Map.empty, 1)
            services' = Map.insert num service services
            ls' = Map.insert line services' ls
         in (num, Services ls')

invalidSteps :: [Station] -> Line -> Network -> [(Station, Station)]
invalidSteps [] _ _ = []
invalidSteps [s] _ _ = [(s, s)]
invalidSteps stations@(start:rest) line network =
        let steps = (last stations, start) : zip stations rest
         in filter invalid steps
        where invalid (a, b) =
                notElem b $ getAdjacentStationsOnLine a line network

isServiceValid :: [Station] -> Line -> Network -> Bool
isServiceValid [] _ _ = False
isServiceValid [_] _ _ = False
isServiceValid stations line network =
        null $ invalidSteps stations line network

data ReadState = ReadState Services (Maybe Line)

readServicesFile :: FilePath -> IO Services
readServicesFile path = do
        content <- readFile path
        return $ readServicesCommands content

readServicesCommands :: String -> Services
readServicesCommands content =
        let commands = filter (/="") $ lines content
            initialState = ReadState Tracks.Service.empty Nothing
            ReadState services _ = foldl readServiceCommand initialState commands
         in services

readServiceCommand :: ReadState -> String -> ReadState
readServiceCommand (ReadState services (Just line)) ('+':service) =
        let names = splitOn "," service
            stations = fmap Station names
            (_, services') = addService stations line services
         in ReadState services' (Just line)

readServiceCommand (ReadState _ Nothing) ('+':_) =
        error "No line to add service to"

readServiceCommand (ReadState services _) lineName =
        ReadState services (Just $ Line lineName)
