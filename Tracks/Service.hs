{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Service (
      Services
    , Tracks.Service.empty
    , getLineServices
    , getLineService

    , addService

    , readServicesFile
    , readServicesCommands
    ) where

import Tracks.Network
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

data Services = Services (Map Line (Map Int [Station]))

instance Show Services where
        show (Services lines) =
            unlines $ fmap showLine $ Map.assocs lines
            where showLine ((Line name), services) =
                    let servicesInfo = intercalate ", " $ fmap showService $ Map.assocs services
                     in name ++ " (" ++ servicesInfo ++ ")"
                  showService (id, stations) =
                      (show id) ++ "-" ++ (show $ length stations)


empty :: Services
empty = Services Map.empty

getLineServices :: Services -> Line -> [Int]
getLineServices (Services lines) line =
        case Map.lookup line lines of
            Just services -> Map.keys services
            Nothing     -> []

getLineService :: Services -> Line -> Int -> [Station]
getLineService (Services lines) line i =
        case Map.lookup line lines of
            Just services -> case Map.lookup i services of
                               Just service -> service
                               Nothing    -> []
            Nothing     -> []

addService :: Services -> Line -> [Station] -> (Int, Services)
addService (Services lines) line service =
        let (services, id) = case Map.lookup line lines of
                               Just services -> let (id, _) = Map.findMax services
                                               in (services, id)
                               Nothing     -> (Map.empty, 1)
            services' = Map.insert id service services
            lines' = Map.insert line services' lines
         in (id, Services lines')

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
            (_, services') = addService services line stations
         in ReadState services' (Just line)

readServiceCommand (ReadState _ Nothing) ('+':_) =
        error "No line to add service to"

readServiceCommand (ReadState services _) lineName =
        ReadState services (Just $ Line lineName)
