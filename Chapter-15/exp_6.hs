sqroot :: Double -> Double
sqroot x | x < 0 = error "Only take positive number"
         | otherwise = 1
