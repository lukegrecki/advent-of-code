import Data.Char
import Data.Tuple
import Data.List
import Data.List.Split

type Object = String
type OrbitingObject = Object
type OrbitedObject = Object
type Root = Object
type DirectOrbit = (OrbitingObject, OrbitedObject)
type IndirectOrbit = (OrbitingObject, OrbitedObject)
type Orbit = (OrbitingObject, OrbitedObject)
type Length = Int

toTuple :: [String] -> (String, String)
toTuple (x1:x2:xs) = (x1, x2)

parseOrbit :: String -> DirectOrbit
parseOrbit s = toTuple (splitOn ")" s)

toIndirectOrbits :: [DirectOrbit] -> Orbit -> [IndirectOrbit]
toIndirectOrbits directOrbits (o1, o2) = [ (o1, o4) | (o3, o4) <- directOrbits, o2 == o3, o2 /= o4 ]

allOrbitsOfLengthLessThan :: Length -> [DirectOrbit] -> [Orbit] -> [Orbit]
allOrbitsOfLengthLessThan 1 directOrbits _ = directOrbits
allOrbitsOfLengthLessThan l directOrbits orbits =
    let shorterOrbits = nub $ allOrbitsOfLengthLessThan (l - 1) directOrbits orbits
        longerOrbits = concatMap (toIndirectOrbits directOrbits) shorterOrbits
    in case longerOrbits of
        [] -> shorterOrbits
        o -> shorterOrbits ++ o

main = do
    contents <- getContents
    let directOrbits = map parseOrbit (lines contents)
        greatestPossibleOrbitLength = length directOrbits
        allOrbits = allOrbitsOfLengthLessThan greatestPossibleOrbitLength directOrbits directOrbits
    print $ length (nub allOrbits)
