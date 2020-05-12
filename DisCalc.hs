{- Discriminant Calculator
 -}

module DisCalc where

{- -----------------------------------------------------------------
 - findQ
 - -----------------------------------------------------------------
 - Description: takes the arguments a, b and c then solves for the q value
 - which is needed in order to get the discriminat and the roots.
 -}
findQ :: Double -> Double -> Double -> Double
findQ a b c = ((3*a*c) - b^2)/(9*a^2)
