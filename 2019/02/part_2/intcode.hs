import Data.Maybe
import Data.Char
import Data.List
import Data.List.Split

type Noun = Int
type Verb = Int
type Memory = [Int]
type OpCode = Int
type Address = Int
type InstructionPointer = Int
type Instructions = [Int]
type Value = Int

readMemory :: String -> Memory
readMemory = map read . splitOn ","

writeMemory :: Memory -> String
writeMemory = intercalate "," . map show

writeOutput :: Memory -> String
writeOutput = show . head

replaceMemoryAt :: Address -> Value -> Memory -> Memory
replaceMemoryAt a v m = 
    let memoryStart = take a m
        memoryEnd = drop (a + 1) m
    in memoryStart ++ [v] ++ memoryEnd

restoreGravityAssistMemory :: Noun -> Verb -> Memory -> Memory
restoreGravityAssistMemory n v m = replaceMemoryAt 2 v (replaceMemoryAt 1 n m)

runProgram1 :: Address -> Memory -> Memory
runProgram1 a m =
    let newValue = m !! (m !! (a + 1)) + m !! (m !! (a + 2))
        position = m !! (a + 3)
    in replaceMemoryAt position newValue m

runProgram2 :: Address -> Memory -> Memory
runProgram2 a m =
    let newValue = m !! (m !! (a + 1)) * m !! (m !! (a + 2))
        position = m !! (a + 3)
    in replaceMemoryAt position newValue m 

runProgram :: InstructionPointer -> Instructions -> Maybe Memory
runProgram pointer instructions =
    let opCode = instructions !! pointer
        nextPointer = pointer + 4
    in case opCode of
        1 -> runProgram nextPointer (runProgram1 pointer instructions)
        2 -> runProgram nextPointer (runProgram2 pointer instructions)
        99 -> Just instructions
        _ -> Nothing

main = do
    contents <- getContents
    let noun = 12
        verb = 2
        memory = readMemory contents
        startAddress = 0
        gravityAssistMemory = restoreGravityAssistMemory noun verb memory
        newMemory = runProgram startAddress gravityAssistMemory
    case newMemory of
        Just m -> putStrLn $ writeOutput m
        _ -> return ()
