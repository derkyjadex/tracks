{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}

module Tracks.Signals where

import Tracks.Network
import Tracks.Service

import Control.Monad.STM
import Control.Concurrent
import STMContainers.Set (Set)
import qualified STMContainers.Set as Set
import Data.Hashable
import GHC.Generics

data Block = Between Station Station Line
           | Platform Station Station Line
           deriving (Show, Ord, Eq, Generic)

instance Hashable Block

data Signals = Signals (Set Block)

clear :: STM Signals
clear = do
        blocks <- Set.new
        return $ Signals blocks

isClear :: Block -> Signals -> STM Bool
isClear block (Signals blocks) = do
        occupied <- Set.lookup block blocks
        return $ not occupied

setOccupied :: Block -> Signals -> STM ()
setOccupied block (Signals blocks) =
        Set.insert block blocks

setClear :: Block -> Signals -> STM ()
setClear block (Signals blocks) =
        Set.delete block blocks
