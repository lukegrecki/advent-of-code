import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Set as Set

type Direction = Char
type Magnitude = Int
type Move = (Direction, Magnitude)
type Path = [Move]
type X = Int
type Y = Int
type Position = (X, Y)
type Trajectory = [Position]
type TrajectoryPositions = [Position]
type Distance = Int

parseToken :: String -> Move
parseToken (d:m) = (d, read m)

parsePath :: String -> Path
parsePath s = 
    let tokens = splitOn "," s
    in map parseToken tokens

readPaths :: String -> [Path]
readPaths s = map parsePath (lines s)

writePath :: Path -> String
writePath = intercalate "," . map show

move :: Position -> Move -> [Position]
move (x, y) ('U', m) = [ (x, y + i) | i <- [1,2..m] ]
move (x, y) ('D', m) = [ (x, y - i) | i <- [1,2..m] ]
move (x, y) ('L', m) = [ (x - i, y) | i <- [1,2..m] ]
move (x, y) ('R', m) = [ (x + i, y) | i <- [1,2..m] ]

toTrajectory :: Path -> Trajectory
toTrajectory p = 
    let start = [(0, 0)]
    in tail $ concat (scanl (move . last) start p)

manhattanDistance :: (Position, Position) -> Int
manhattanDistance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

distanceFromOrigin :: Position -> Int
distanceFromOrigin p = 
    let origin = (0, 0)
    in manhattanDistance (origin, p)

isIntersection :: (Position, Position) -> Bool
isIntersection (p1, p2) = manhattanDistance (p1, p2) == 0

findClosestIntersection :: [TrajectoryPositions] -> Position
findClosestIntersection (t1:t2:_) = fst . head $ filter isIntersection [ (p1, p2) | p1 <- t1, p2 <- t2]

findCloserPosition :: Position -> Position -> Ordering
findCloserPosition p1 p2 = compare (distanceFromOrigin p1) (distanceFromOrigin p2)

main = do
    contents <- getContents
    let paths = readPaths contents
        trajectories = map toTrajectory paths
        trajectoryPositions = map (sortBy findCloserPosition) trajectories
        closestIntersection = findClosestIntersection trajectoryPositions
    print $ distanceFromOrigin closestIntersection
