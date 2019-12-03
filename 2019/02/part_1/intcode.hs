import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split

replaceIndexWith :: Int -> Int -> [Int] -> [Int]
replaceIndexWith index newValue list = 
    let listStart = take index list
        listEnd = drop (index + 1) list
    in listStart ++ [newValue] ++ listEnd

runProgram1 :: [Int] -> [Int]
runProgram1 p =
    let sum = p !! (p !! 1) + p !! (p !! 2)
        position = p !! 3
    in replaceIndexWith position sum p

runProgram2 :: [Int] -> [Int]
runProgram2 p =
    let product = p !! (p !! 1) * p !! (p !! 2)
        position = p !! 3
    in replaceIndexWith position product p

runProgram :: [Int] -> Maybe [Int]
runProgram p
    | opCode == 1 = Just $ runProgram1 p
    | opCode == 2 = Just $ runProgram2 p
    | otherwise = Nothing
    where opCode = head p

main = do
    contents <- getContents
    let program = map read (splitOn "," contents)
        output = runProgram program
    if isJust output 
        then putStrLn $ intercalate "," $ map show (fromJust output)
        else return ()