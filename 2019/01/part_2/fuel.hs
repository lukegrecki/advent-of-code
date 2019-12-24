import Data.Char

type Mass = Int
type Fuel = Int

calculateFuelRequired :: Mass -> Fuel
calculateFuelRequired m
    | fuelRequired > 0 = fuelRequired + calculateFuelRequired fuelRequired
    | otherwise = 0
    where fuelRequired = floor $ (fromIntegral m :: Float) / 3 - 2

main = do
    contents <- getContents
    let allLines = lines contents
        fuelAmounts = map (calculateFuelRequired . read) allLines
        totalFuelAmount = sum fuelAmounts
    print totalFuelAmount
