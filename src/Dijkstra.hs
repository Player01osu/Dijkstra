module Dijkstra
  ( Vertex
  , Distance
  , Edge
  , Graph
  , DistanceMap
  , shortestPath
  , graphA
  , graphB )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe (catMaybes, fromJust)
import Data.List (sortBy)
import Debug.Trace (traceShowId, trace)
import Text.Printf (printf)

type Distance = Int

type Vertex = (Int, Int)

type Edge = (Vertex, Vertex, Distance)

type Graph = M.Map Vertex [(Vertex, Distance)]

type DistanceMap = M.Map Vertex (Maybe (Vertex, Distance))

type VertexQueue = S.Set (Distance, Vertex)

graphFromEdgeDistance :: [Edge] -> Graph
graphFromEdgeDistance edgeList = go edgeList M.empty
  where
    go :: [Edge] -> Graph -> Graph
    go [] graph = graph
    go ((vertexA, vertexB, distance):xs) graph =
      let updatedGraph =
            M.insertWith (++) vertexB [(vertexA, distance)] $
            M.insertWith (++) vertexA [(vertexB, distance)] graph
       in go xs updatedGraph

shortestPath :: Vertex -> Vertex -> Graph -> Maybe ([Vertex], Distance)
shortestPath from to graph = pathDistance
  where
    tree = (shortestPathTree from graph)
    pathDistance = case tree M.! to of
      Just (vertex, distance) -> Just $ (from:buildPath vertex [to], distance)
      Nothing -> Nothing
    buildPath vertex acc
      | vertex == from = acc
      | otherwise = buildPath (getVertex $ fromJust $ tree M.! vertex) (vertex:acc)
    getVertex (vertex, _) = vertex

shortestPathTree :: Vertex -> Graph -> DistanceMap
shortestPathTree from graph = go (S.singleton (0, from)) S.empty initDistanceMap
  where
    initDistanceMap =
      M.insert from (Just (from, 0)) $ M.fromList $ map (\k -> (k, Nothing)) $ M.keys graph

    go :: VertexQueue -> S.Set Vertex -> DistanceMap -> DistanceMap
    go pqueue visited distanceMap
      | S.null pqueue = distanceMap
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
          in (go newQueue newVisited newMap)

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

-- a-b 7, a-c 9, b-c 10, b-d 15, c-d 11, d-e 6, e-f 9, f-a 14, f-c 2
graphA =
  let (a, b, c, d, e, f) = ((1, 0), (2, 11), (3, 7), (4, 0), (5, 10), (6, 8))
      (ab, ac, bc, bd, cd, de, ef, fa, fc) = (7, 9, 10, 15, 11, 6, 9, 14, 2)
   in graphFromEdgeDistance
        [ (a, b, ab)
        , (a, c, ac)
        , (b, c, bc)
        , (b, d, bd)
        , (c, d, cd)
        , (d, e, de)
        , (e, f, ef)
        , (f, a, fa)
        , (f, c, fc)
        , ((7, 0), (8, 0), 1)
        ]

--
-- 1  2  3  4  5  6  7  8  9  10
-- 11 12 13 14 15 16 17 18 19 20
-- 21 22 23 24 25 26 27 28 29 30
-- 31 32 33 34 35 36 37 38 39 40
-- 41 42 43 44 45 46 47 48 49 50
-- 51 52 53 54 55 56 57 58 59 60
-- 61 62 63 64 65 66 67 68 69 70
--
graphB = graphFromEdgeDistance edges
  where
    (minX, minY) = (1, 1)
    (maxX, maxY) = (100, 100)
    vertices :: [Vertex]
    vertices = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
    edges = concat $ map createRelations vertices
    createRelations :: Vertex -> [Edge]
    createRelations (x, y) =
      [ ((x, y), (x + 0, y + 1), 1)
      , ((x, y), (x - 1, y + 0), 1)
      , ((x, y), (x + 1, y - 1), 1)
      , ((x, y), (x + 0, y - 1), 1)
      , ((x, y), (x + 1, y + 1), 1) ]

main = putStrLn $ show $ shortestPath (0,9) (0,80) graphB
