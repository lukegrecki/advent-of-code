import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Foldable as Foldable

type Direction = Char
type Magnitude = Int
type Move = (Direction, Magnitude)
type Path = [Move]
type X = Int
type Y = Int
type Position = (X, Y)
type Trajectory = [Position]
type TrajectoryPositions = [Position]
type SignalDelay = Int
type PositionDelay = (Position, SignalDelay)
type TrajectoryDelay = [PositionDelay]

parseToken :: String -> Move
parseToken (d:m) = (d, read m)

parsePath :: String -> Path
parsePath s = 
    let tokens = splitOn "," s
    in map parseToken tokens

readPaths :: String -> [Path]
readPaths s = map parsePath (lines s)

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

isIntersection :: (Position, Position) -> Bool
isIntersection (p1, p2) = manhattanDistance (p1, p2) == 0

signalDelay :: Trajectory -> SignalDelay
signalDelay = length

toSignalDelays :: Trajectory -> [SignalDelay]
toSignalDelays t = [1,2..signalDelay t]

toTrajectoryDelay :: (Trajectory, [SignalDelay]) -> TrajectoryDelay
toTrajectoryDelay (t, sd) = zip t sd

findShortestDelayWithin :: SignalDelay -> [TrajectoryDelay] -> Maybe SignalDelay
findShortestDelayWithin sd (td1:td2:ts) = Just $ head [ sd1 + sd2 | (p1, sd1) <- td1, (p2, sd2) <- td2, sd1 + sd2 <= sd && isIntersection (p1, p2)]
findShortestDelayWithin _ _ = Nothing


main = do
    contents <- getContents
    let paths = readPaths contents
        trajectories = map toTrajectory paths
        signalDelays = map toSignalDelays trajectories
        longestDelay = sum $ map last signalDelays
        trajectoryDelays = zipWith (curry toTrajectoryDelay) trajectories signalDelays
        shortestDelay = findShortestDelayWithin longestDelay trajectoryDelays
    Foldable.forM_ shortestDelay print