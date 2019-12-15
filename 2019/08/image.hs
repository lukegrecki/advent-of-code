import Data.Char
import Data.List
import Data.List.Split

type Width = Int
type Height = Int
type Index = Int
type Length = Int
type Dimensions = (Width, Height)
type Layer = [Int]

rstrip :: String -> String
rstrip = reverse . dropWhile isSpace . reverse

charToString :: Char -> String
charToString c = [c]

parseLayer :: String -> Layer
parseLayer = map $ read . charToString

parseLayers :: Dimensions -> String -> [Layer]
parseLayers (w, h) image =
    let imageLength = length image
        layerLength = w * h
        layerStrings = chunksOf layerLength image
    in map parseLayer layerStrings

toDigitCount :: Int -> Layer -> Int
toDigitCount i l = length $ filter (==i) l

main = do
    contents <- getContents
    let width = 25
        height = 6
        dimensions = (width, height)
        image = rstrip contents
        layers = parseLayers dimensions image
        lessZerosThan a b = toDigitCount 0 a `compare` toDigitCount 0 b
        minimumZerosLayer = minimumBy lessZerosThan layers
    print $ toDigitCount 1 minimumZerosLayer * toDigitCount 2 minimumZerosLayer