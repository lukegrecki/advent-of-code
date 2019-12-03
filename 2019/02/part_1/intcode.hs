import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

runProgram1 :: [Int] -> [Int]
runProgram1 p =
    let sum = p !! 1 + p !! 2
        position = p !! 2
    in (take position p) ++ [sum] ++ drop (position + 1) p

runProgram2 :: [Int] -> [Int]
runProgram2 p =
    let product = p !! 1 * p !! 2
        position = p !! 2
    in (take position p) ++ [product] ++ drop (position + 1) p

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
        then putStrLn $ intercalate ", " (map show (fromJust output))
        else return ()