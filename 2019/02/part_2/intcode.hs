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
type Output = Int
type Target = Int
type ParameterSet = (Noun, Verb)
type OutputSet = (ParameterSet, Output)

readMemory :: String -> Memory
readMemory = map read . splitOn ","

writeMemory :: Memory -> String
writeMemory = intercalate "," . map show

takeOutput :: Memory -> Output
takeOutput = head

writeOutput :: Memory -> String
writeOutput = show . takeOutput

replaceMemoryAt :: Address -> Value -> Memory -> Memory
replaceMemoryAt a v m = 
    let memoryStart = take a m
        memoryEnd = drop (a + 1) m
    in memoryStart ++ [v] ++ memoryEnd

restoreGravityAssistMemory :: Memory -> ParameterSet -> Memory
restoreGravityAssistMemory m (n, v) = replaceMemoryAt 2 v (replaceMemoryAt 1 n m)

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

getOutput :: Memory -> ParameterSet -> Maybe OutputSet
getOutput memory parameterSet = let startAddress = 0
                                    gravityAssistMemory = restoreGravityAssistMemory memory parameterSet
                                    newMemory = runProgram startAddress gravityAssistMemory
                                in case newMemory of
                                    Just m -> Just (parameterSet, takeOutput m)
                                    _ -> Nothing

checkTarget :: Target -> Maybe OutputSet -> Bool
checkTarget t (Just ((n, v), o)) = o == t
checkTarget _ _ = False

findSolution :: Memory -> Target -> [Noun] -> [Verb] -> Maybe OutputSet
findSolution memory target nouns verbs =
    let parameterSets = [ (n, v) | n <- nouns, v <- verbs ]
        outputSets = map (getOutput memory) parameterSets
    in head $ filter (checkTarget target) outputSets

main = do
    contents <- getContents
    let target = 19690720
        nouns = [1,2..99]
        verbs = [1,2..99]
        memory = readMemory contents
        solution = findSolution memory target nouns verbs 
    case solution of
        Just ((noun, verb), output) -> print (100 * noun + verb)
        _ -> return ()
