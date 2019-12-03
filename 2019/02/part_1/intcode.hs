import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe

-- runProgram1 :: [Int] -> [Int]
-- runProgram1 p =
--     let sum = p !! 0 + p !! 1
--         position = p !! 2
--     (take position p) ++ [sum] ++ (drop position + 1 p)

runProgram :: [Int] -> Maybe [Int]
runProgram p
    | opCode == 1 = Just p
    | otherwise = Nothing
    where opCode = head p

main = do
    contents <- getContents
    let program = map read (splitOn "," contents)
        output = runProgram program
    if isJust output 
        then putStrLn $ intercalate ", " (map show (fromJust $ output))
        else return ()