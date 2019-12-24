import Data.Char
import Data.List

type Password = Int
type PasswordDigits = [Int]

digits :: Password -> PasswordDigits
digits = map digitToInt . show

isMonotonic :: PasswordDigits -> Bool
isMonotonic (x1:x2:xs) = isMonotonic (x2:xs) && (x2 - x1 >= 0)
isMonotonic _ = True

hasValidGroups :: PasswordDigits -> Bool
hasValidGroups pd =
    let requiredGroupSize = 2
    in requiredGroupSize `elem` map length (group pd)

isValidPassword :: Password -> Bool
isValidPassword i =
    let d = digits i
    in isMonotonic d && hasValidGroups d

main = do
    let lowerBound = 145852
        upperBound = 616942
        passwordRange = [lowerBound, lowerBound + 1..upperBound]
        validPasswords = filter isValidPassword passwordRange
    print $ length validPasswords
