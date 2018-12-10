module Day6 (maxArea) where

import Data.List (minimumBy, maximumBy, sortBy)
import Data.Ord (comparing)
import Data.Map

type Coordinate = (Int, Int)
type Location = Coordinate
type Locations = [Coordinate]
type Grid = [Coordinate]
data GridLimit =
  GridLimit {
    minX :: Int
  , maxX :: Int
  , minY :: Int
  , maxY :: Int
  } deriving (Eq, Show)

limits :: Locations -> GridLimit
limits xs =
  GridLimit {
    minX = (fst $ minimumBy (comparing fst) xs) - 1
  , maxX = (fst $ maximumBy (comparing fst) xs) + 1
  , minY = (snd $ minimumBy (comparing snd) xs) - 1
  , maxY = (snd $ maximumBy (comparing snd) xs) + 1
  }

-- grid upto min and max elements (limits^)
grid :: GridLimit -> Grid
grid gl =
  [(x, y) |
   x <- [(minX gl)..(maxX gl)], y <- [(minY gl)..(maxY gl)]]

-- outline of grid - to eliminate infinites
outline :: GridLimit -> [Coordinate]
outline gl =
  let up    = [(maxX gl, y) | y <- [(minY gl)..(maxY gl)]]
      down  = [(minX gl, y) | y <- [(minY gl)..(maxY gl)]]
      left  = [(x, minY gl) | x <- [(minX gl)..(maxX gl)]]
      right = [(x, maxY gl) | x <- [(minX gl)..(maxX gl)]]
  in concat [up, down, left, right]

-- Manhatten distance between any two coordiantes
distance :: Coordinate -> Coordinate -> Int
distance (x1, y1) (x2, y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

-- Return closest location from list
-- `Nothing` if two points at same distance (in problem definition)
closest :: Locations -> Coordinate -> Maybe Location
closest coords c =
  let dists = Prelude.map (\p -> (distance c p, p)) coords
      (p1:p2:_) = sortBy (comparing fst) dists
  in
    if fst p1 == fst p2 -- at same distance from two points then ignore
    then Nothing
    else Just $ snd p1 -- min

-- Map of given coordinates - list of all its closest coordinates
-- ie. area of each given coordinate
calcArea :: Grid -> Locations -> Map Location [Coordinate] -> Map Location [Coordinate]
calcArea [] _ acc = acc
calcArea (x:xs) cs acc =
  case closest cs x of
    Nothing -> calcArea xs cs acc
    Just k  -> calcArea xs cs (insertWith (++) k [x] acc)

-- deletes locations from Map that are closest from edges (these are infinites)
deleteEdges :: [Coordinate] -> Map Location [Coordinate] -> Map Location [Coordinate]
deleteEdges [] mc = mc
deleteEdges (x:xs) mc = deleteEdges xs (Data.Map.filter (notElem x) mc)

maxArea :: Locations -> Int
maxArea ps =
  let gl = limits ps
      gridMap = calcArea (grid gl) ps empty
      innerArea = deleteEdges (outline gl) gridMap
  in snd $ maximumBy (comparing snd) . toList $ Data.Map.map length innerArea
