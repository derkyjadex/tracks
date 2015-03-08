{-# LANGUAGE NamedFieldPuns #-}

module Tracks.Layout where

import Tracks.Network

import System.Random
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub)

type Coord = (Float, Float)
type Vector = (Float, Float)

type VertexState = (Vector, Coord)

width, height, area, cooling :: Float
width = 1920
height = 1080
area = width * height
cooling = 0.99

type Vertex = Station

data Graph = Graph { vertices :: [Vertex]
                   , adjacency :: Map Vertex [Vertex]
                   , vertexStates :: Map Vertex VertexState
                   , colours :: Map Vertex Int
                   , k :: Float
                   , temperature :: Float
                   , minTemperature :: Float
                   , n :: Int
                   }

edges :: Graph -> [(Vertex, Vertex)]
edges g =
        let adj = Map.toList $ adjacency g
            es = fmap order $ concatMap flatten adj
         in nub es
      where flatten (a, bs) = zip (repeat a) bs
            order (a, b) = if a > b then (b, a) else (a, b)

vertexPosition :: Graph -> Vertex -> Coord
vertexPosition g v =
        let Just (_, coords) = Map.lookup v $ vertexStates g
         in coords

vertexColour :: Graph -> Vertex -> Int
vertexColour g v =
        let Just c = Map.lookup v $ colours g
         in c

new :: [Vertex] -> (Vertex -> [Vertex]) -> Graph
new vs getAdjacent =
        let getAdjacency v = (v, getAdjacent v)
            maxT = 0.1 * (min width height)
            minT = 0.1 * maxT
         in Graph { vertices = vs
                  , adjacency = Map.fromList $ fmap getAdjacency vs
                  , vertexStates = Map.fromList $ zip vs $ zip (repeat (0, 0)) randomCoords
                  , colours = Map.fromList $ zip vs randomColours
                  , k = 0.07 * sqrt (area / (fromIntegral $ length vs))
                  , temperature = maxT
                  , minTemperature = minT
                  , n = 0
                  }
      where randomCoords =
                let (xGen, yGen) = split $ mkStdGen 6734098
                    xs = randomRs (0, width) xGen
                    ys = randomRs (0, height) yGen
                 in zip xs ys
            randomColours =
                let gen = mkStdGen 49803
                 in randomRs (0, 359) gen
delta :: Coord -> Coord -> Vector
delta (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

norm :: Vector -> Float
norm (x, y) = sqrt (x * x + y * y)

normal :: Vector -> Vector
normal (x, y) =
        let n = norm (x, y)
         in (x / n, y / n)

scale :: Vector -> Float -> Vector
scale (x, y) a = (a * x, a * y)

add :: Vector -> Vector -> Vector
add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


attractiveForce :: Float -> Float -> Float
attractiveForce k d = (d * d) / k

repulsiveForce :: Float -> Float -> Float
repulsiveForce k d = -(k * k) / d


vertexDelta :: Graph -> Vertex -> Vertex -> Vector
vertexDelta g v u =
        let pv = vertexPosition g v
            pu = vertexPosition g u
         in delta pv pu

vertexRepulsions :: Graph -> Vertex -> [Vector]
vertexRepulsions g v =
        let vs = filter (/=v) $ vertices g
         in fmap disp vs
      where disp u =
                let d = vertexDelta g v u
                    f = repulsiveForce (k g) (norm d)
                 in scale (normal d) f

vertexAttractions :: Graph -> Vertex -> [Vector]
vertexAttractions g v =
        let vs = fromMaybe undefined $ Map.lookup v $ adjacency g
         in fmap disp vs
      where disp u =
                let d = vertexDelta g v u
                    f = attractiveForce (k g) (norm d)
                 in scale (normal d) f

vertexForces :: Graph -> Vertex -> Vector
vertexForces g v =
        let fs = vertexRepulsions g v ++ vertexAttractions g v
         in foldr add (0, 0) fs

applyForces :: Graph -> Graph
applyForces g =
        g { vertexStates = Map.mapWithKey newState $ vertexStates g
          , temperature = max (minTemperature g) (cooling * temperature g)
          , n = n g + 1
          }
        where newState =
                  if (n g `div` 1000) `mod` 2 == 0
                      then newStateA
                      else newStateB
              newStateA v (_, (x, y)) =
                let t = temperature g
                    d = vertexForces g v
                    (dx, dy) = scale (normal d) (0.2 * (min (norm d) t))
                    x' = max 0 $ min width $ x + dx
                    y' = max 0 $ min height $ y + dy
                 in ((0, 0), (x', y'))
              newStateB v (vel, (x, y)) =
                let f = vertexForces g v
                    (dx, dy) = vel `add` (f `scale` 0.0001)
                    x' = max 0 $ min width $ x + dx
                    y' = max 0 $ min height $ y + dy
                 in ((dx, dy), (x', y'))

buildGraph :: Network -> Graph
buildGraph network =
        let vs = getStations network
            getAdjacent v = nub $ fmap snd $ getAdjacentStations v network
         in new vs getAdjacent

layout :: Network -> [(Station, Coord)]
layout network =
        let initial = buildGraph network
            iterations = iterate applyForces initial
            final = iterations !! 2401
         in Map.toList $ fmap snd $ vertexStates final
