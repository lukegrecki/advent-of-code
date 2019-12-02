import Data.Char  

type Mass = Float
type Fuel = Int

calculateFuelRequired :: Mass -> Fuel
calculateFuelRequired m = floor (m / 3) - 2 

main = do  
    contents <- getContents  
    let allLines = lines contents
        fuelAmounts = map calculateFuelRequired (map read allLines)
        totalFuelAmount = sum fuelAmounts
    putStrLn $ show totalFuelAmount
