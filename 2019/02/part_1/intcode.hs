import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split

readProgram :: String -> [Int]
readProgram = map read . splitOn ","

writeProgram :: [Int] -> String
writeProgram = intercalate "," . map show

restoreGravityAssistProgram :: [Int] -> [Int]
restoreGravityAssistProgram p = replaceIndexWith 2 2 (replaceIndexWith 1 12 p)

replaceIndexWith :: Int -> Int -> [Int] -> [Int]
replaceIndexWith index newValue list = 
    let listStart = take index list
        listEnd = drop (index + 1) list
    in listStart ++ [newValue] ++ listEnd

runProgram1 :: Int -> [Int] -> [Int]
runProgram1 i p =
    let sum = p !! (p !! (i + 1)) + p !! (p !! (i + 2))
        position = p !! (i + 3)
    in replaceIndexWith position sum p

runProgram2 :: Int -> [Int] -> [Int]
runProgram2 i p =
    let product = p !! (p !! (i + 1)) * p !! (p !! (i + 2))
        position = p !! (i + 3)
    in replaceIndexWith position product p 

runProgram :: Int -> Maybe [Int] -> Maybe [Int]
runProgram i p
    | isJust p =
        let program = fromJust p
            opCode = program !! i
            nextIndex = i + 4
        in case opCode of
            1 -> runProgram nextIndex (Just $ runProgram1 i program)
            2 -> runProgram nextIndex (Just $ runProgram2 i program)
            99 -> Just program
            _ -> Nothing
    | otherwise = Nothing        

main = do
    contents <- getContents
    let program = readProgram contents
        gravityAssistProgram = restoreGravityAssistProgram program
        output = runProgram 0 (Just gravityAssistProgram)
    case output of
        Just o -> print $ head o
        _ -> return ()
