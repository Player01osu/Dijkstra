import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace (trace, traceShowId)
import Data.Maybe (fromJust, isJust)
import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Graphics.Gloss.Data.Color
import Dijkstra
  ( shortestPath
  , Part
  , DistanceMap
  , shortestPathDistanceMap
  , shortestPathTreePart )
import Graphs (graphB)
import GHC.Float (int2Float)

nodeSize = 40
nodeX = 80
nodeY = 80

graph = graphB
from = (1, 1)
to = (40, 30)

drawRelationLines origin (coord, _) = Line [origin, nodeLocation coord]

nodeLocation (x, y) = ((int2Float x) * nodeX, (int2Float y) * nodeY)

drawNode thickness (x, y) = Translate x y (thickCircle thickness nodeSize)

drawGraph = color black $ Pictures $ graphNodes
  where
    graphNodes = map drawNodeRelation $ M.assocs graph

    drawNodeRelation (coord, relations) =
      let (x, y) = nodeLocation coord
          node = drawNode 0.0 (x, y)
          relationLines = map (drawRelationLines (x, y)) relations
      --in Pictures $ [node]
      in Pictures $ node:relationLines

drawSolution = color red $ pictures [solutionNodes, solutionLine]
  where
    (vertices, _) = fromJust $ shortestPath from to graph
    solutionNodes = pictures $ map (drawNode 4.0 . nodeLocation) vertices
    solutionLine = line $ map nodeLocation vertices

drawPartial :: Part -> Picture
drawPartial (pqueue, visited, tree) = nodes
  where
    --vertices =
    --  concat [[a, b] | (a, Just (b, _)) <- M.assocs tree,
    --    a == b ]

    vertices = [Color (makeColorI
                        (double2Int $ d * 10)
                        (double2Int $ d * 2)
                        (double2Int $ d * 4) 255) (drawNode 4.0 $ nodeLocation a)
                   | (a, Just (_, d)) <- M.assocs tree,
                    let b = fromJust $ tree M.! a]
                    --let (_, d) = b]
                  -- | (a <- S.toList visited,
                  --  let b = fromJust $ tree M.! a,
                  --  let (_, d) = b]
    nodes = pictures vertices

drawComplete tree = color red $ pictures [solutionNodes, solutionLine]
  where
    (vertices, _) = fromJust $ shortestPathDistanceMap from to tree
    solutionNodes = pictures $ map (drawNode 4.0 . nodeLocation) vertices
    solutionLine = line $ map nodeLocation vertices


data World = World (Either Part DistanceMap)
  deriving (Show)

drawWorld world = Pictures [drawGraph, drawPart world]
  where
    drawPart (World (Left part)) = drawPartial part
    drawPart (World (Right tree)) = drawComplete tree

nextState viewport time w@(World (Right tree)) = w
nextState viewport time (World (Left part)) = World (shortestPathTreePart from graph (Just part))

drawMain = Pictures [drawGraph, drawSolution]

--main = display (InWindow "TEST" (640, 480) (0, 0)) white drawMain


--main =
--  play
--  (InWindow "TEST" (640, 480) (0, 0))
--  white
--  1
--  (World True)
--  drawWorld
--  handleEvent
--  nextState

main =
  simulate
  (InWindow "TEST" (640, 480) (0, 0))
  white
  60
  (World $ shortestPathTreePart from graph Nothing)
  drawWorld
  nextState
