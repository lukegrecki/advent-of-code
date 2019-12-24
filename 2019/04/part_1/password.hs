import Data.Char

digits :: Int -> [Int]
digits = map digitToInt . show

hasAdjacentDigits :: [Int] -> Bool
hasAdjacentDigits (x1:x2:xs) = hasAdjacentDigits (x2:xs) || (x1 == x2)
hasAdjacentDigits _ = False

isMonotonic :: [Int] -> Bool
isMonotonic (x1:x2:xs) = isMonotonic (x2:xs) && (x2 >= x1)
isMonotonic _ = True

isValidPassword :: Int -> Bool
isValidPassword i =
    let d = digits i
    in hasAdjacentDigits d && isMonotonic d

main = do
    let lowerBound = 145852
        upperBound = 616942
        passwordRange = [lowerBound, lowerBound + 1..upperBound]
        validPasswords = filter isValidPassword passwordRange
    print $ length validPasswords
