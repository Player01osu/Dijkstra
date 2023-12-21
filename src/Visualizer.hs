import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace (trace, traceShowId)
import Data.Maybe (fromJust)
import Graphics.Gloss
import Graphics.Gloss.Data.Color
import Dijkstra (shortestPath)
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

drawGraph = Pictures $ graphNodes
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
    (vertices, _) = traceShowId $ fromJust $ shortestPath from to graph
    solutionNodes = pictures $ map (drawNode 4.0 . nodeLocation) vertices
    solutionLine = line $ map nodeLocation vertices


data World = World Bool
  deriving (Show)

--drawWorld (World update)
--  | update = graphPicture
--  | otherwise  = Blank

nextState time (World update)
  | update = trace (show time) $ World True
  | otherwise = World True

handleEvent event world = world

drawMain = Pictures [drawGraph, drawSolution]

main = display (InWindow "TEST" (640, 480) (0, 0)) white drawMain


--main =
--  play
--  (InWindow "TEST" (640, 480) (0, 0))
--  white
--  1
--  (World True)
--  drawWorld
--  handleEvent
--  nextState
