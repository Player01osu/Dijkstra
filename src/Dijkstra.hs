module Dijkstra
  ( DistanceMap
  , shortestPath
  , shortestPathTree
  , shortestPathTreePart
  , shortestPathDistanceMap
  , Part
  , VertexQueue
  , Vertex )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy)
import Debug.Trace (traceShowId, trace)
import Text.Printf (printf)
import Graphs
  ( Distance
  , Vertex
  , Edge
  , Graph )

type DistanceMap = M.Map Vertex (Maybe (Vertex, Distance))

type VertexQueue = S.Set (Distance, Vertex)

shortestPath :: Vertex -> Vertex -> Graph -> Maybe ([Vertex], Distance)
shortestPath from to graph = shortestPathDistanceMap from to tree
  where
    tree = (shortestPathTree from graph)

shortestPathDistanceMap :: Vertex -> Vertex -> DistanceMap -> Maybe ([Vertex], Distance)
shortestPathDistanceMap from to tree = pathDistance
  where
    pathDistance = case tree M.! to of
      Just (vertex, distance) -> Just $ (from:buildPath vertex [to], distance)
      Nothing -> Nothing
    buildPath vertex acc
      | vertex == from = acc
      | otherwise = buildPath (getVertex $ fromJust $ tree M.! vertex) (vertex:acc)
    getVertex (vertex, _) = vertex

type Part = (VertexQueue, S.Set Vertex, DistanceMap)

shortestPathTreePart :: Vertex -> Graph -> Maybe Part -> Either Part DistanceMap
shortestPathTreePart from graph Nothing = shortestPathTreePart from graph (Just ((S.singleton (0, from)), S.empty, initDistanceMap))
  where
    initDistanceMap =
      M.insert from (Just (from, 0)) $ M.fromList $ map (\k -> (k, Nothing)) $ M.keys graph
shortestPathTreePart from graph (Just (pqueue, visited, distanceMap)) = go pqueue visited distanceMap
  where
    go :: VertexQueue -> S.Set Vertex -> DistanceMap -> Either Part DistanceMap
    go pqueue visited distanceMap
      | S.null pqueue = Right distanceMap
      | otherwise =
          let ((_, cur), rest) = S.deleteFindMin pqueue
              vertexDistance = graph M.! cur
              (_, curWeight) = fromJust $ distanceMap M.! cur
              newMap         = updateMap vertexDistance cur curWeight distanceMap
              newVisited     = S.insert cur visited
              newQueue       =
                S.union
                rest
                (S.fromList $
                  (filter (\(_, v) -> not $ S.member v visited) $ map (\(v, d) -> (d, v)) vertexDistance))
          in Left (newQueue, newVisited, newMap)

    updateMap vertexDistance cur curWeight distanceMap =
      foldl (insertMin cur curWeight) distanceMap vertexDistance

    insertMin :: Vertex -> Distance -> DistanceMap -> (Vertex, Distance) -> DistanceMap
    insertMin cur curWeight map (v, d) =
      M.adjust (\weight ->
        case weight of
          Just (old, w) -> Just $ chooseMin (old, cur) w (curWeight + d)
          Nothing -> Just $ (cur, curWeight + d)) v map

    chooseMin (l, r) a b
      | a < b = (l, a)
      | otherwise = (r, b)

shortestPathTree :: Vertex -> Graph -> DistanceMap
shortestPathTree from graph = go $ shortestPathTreePart from graph Nothing
  where
    go (Left part) = go $ shortestPathTreePart from graph (Just part)
    go (Right distanceMap) = distanceMap
