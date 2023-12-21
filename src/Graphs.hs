module Graphs
  ( Distance
  , Vertex
  , Edge
  , Graph
  , graphFromEdgeDistance
  , graphA
  , graphB )
where

import qualified Data.Map.Strict as M

type Distance = Double

type Vertex = (Int, Int)

type Edge = (Vertex, Vertex, Distance)

type Graph = M.Map Vertex [(Vertex, Distance)]

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
    (maxX, maxY) = (50, 50)
    vertices :: [Vertex]
    vertices = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
    edges = filter removeBlock $ concat $ map createRelations vertices
    inBlockParam (x, y) xLow xHigh yLow yHigh =
      x < xHigh && x > xLow && y < yHigh && y > yLow
    blockA coord = inBlockParam coord 15 30 20 35
    blockB coord = inBlockParam coord 25 30 5 25
    removeBlock (from, to, _)
      = not ((blockA from && blockA to) || (blockB from && blockB to))
    createRelations :: Vertex -> [Edge]
    createRelations (x, y) =
      [ ((x, y), (x - 1, y + 0), 1)
      , ((x, y), (x + 1, y - 1), 1.141)
      , ((x, y), (x + 0, y - 1), 1)
      , ((x, y), (x + 1, y + 1), 1.141) ]
