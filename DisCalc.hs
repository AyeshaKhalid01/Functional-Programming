{- Discriminant Calculator
 -}

module DisCalc where

{- 
 - findQ
 - -----------------------------------------------------------------
 - Description: takes the arguments a, b and c then solves for the q value
 - which is needed in order to get the discriminat and the roots.
 -}
findQ :: Double -> Double -> Double -> Double
findQ a b c = ((3*a*c) - b^2)/(9*a^2)

{- 
 - findR
 - -----------------------------------------------------------------
 - Description: takes the arguments a, b, c and d then solves for the r value
 - which is needed in order to get the discriminat and solve for the roots.
 -}
findR :: Double -> Double -> Double -> Double -> Double
findR a b c d = ((9*a*b*c)-(27*a^2*d)-(2*b^3))/(54*a^3)
