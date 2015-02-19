{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Train where

import Tracks.Network
import Tracks.Service
import Tracks.Signals

import Control.Monad.STM
import Data.Maybe (fromMaybe)

data Train = Train { trainName :: String
                   , block :: Block
                   , service :: [Station]
                   , signals :: Signals
                   }

instance Show Train where
        show Train { trainName, block, service } =
            trainName ++ "@" ++ show block ++ " -> " ++ show (head service)

placeTrain :: String -> [Station] -> Line -> Signals -> Network -> STM (Maybe Train)
placeTrain name stations@(start:prev:_) line signals network = do
        let platform = Platform prev start line
        platformClear <- isClear platform signals
        if (not platformClear) || (not $ isServiceValid stations line network)
            then return Nothing
            else do
                setOccupied platform signals
                return $ Just Train { trainName = name
                                    , block = platform
                                    , service = drop 1 $ cycle stations
                                    , signals = signals
                                    }

nextBlock :: Train -> Block
nextBlock Train { block = Between prev next line } =
        Platform prev next line

nextBlock Train { block = Platform prev current line, service } =
        let next = head service
         in Between current next line

proceed :: Train -> Block -> STM Bool
proceed train@Train { block, service, signals } next = do
        nextClear <- isClear next signals
        if not nextClear
            then return False
            else do
                setOccupied next signals
                setClear block signals
                return True

stepTrain :: Train -> STM Train
stepTrain train@Train { block = Between _ _ _, service } = do
        let next = nextBlock train
        moved <- proceed train next
        if not moved
            then return train
            else return train { block = next
                              , service = drop 1 service
                              }

stepTrain train@Train { block = Platform _ _ _ } = do
        let next = nextBlock train
        moved <- proceed train next
        if not moved
            then return train
            else return train { block = next }
