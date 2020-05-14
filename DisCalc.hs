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


{- -----------------------------------------------------------------
 - discriminant 
 - -----------------------------------------------------------------
 - Description: takes the arguments q and r which were previously solved for and
 - returns the value of the discriminat  
 -}
 discriminant :: Double -> Double -> Double
 discriminant q r = q^3 + r^2

{- -----------------------------------------------------------------
 - findS
 - -----------------------------------------------------------------
 - Description: takes the arguments q and r which were previously solved for, and returns
 - S which is used to solve for the roots
 -}
 findS :: Double -> Double -> Double
 findS q r = 
    cubicRoot x
    where 
        x = (r + sqrt(q^3 + r^2)) 
    
{- -----------------------------------------------------------------
 - findT
 - -----------------------------------------------------------------
 - Description: takes the arguments q and r which were previously solved for, to obtain 
 - the value of T that then is used to solve for the roots of the function
 -}
 findT :: Double -> Double -> Double
 findT q r = 
    cubicRoot x
    where 
        x = (r - sqrt(q^3 + r^2)) 

{- -----------------------------------------------------------------
 - findSolutions
 - -----------------------------------------------------------------
 - Description: gets the value of the discriminant and based on that solves and returns the roots
 - of the function. 
 -}
findSolutions :: Double -> Double -> Double -> Double -> [Double]
findSolutions a b c d = 
    -- the output is based on the value of the discriminant  
    if dis < 0
        then []
        else if dis == 0
            then [x1, x2, x3]
            else [x1]
    where 
        q = findQ a b c
        r = findR a b c d
        s = findS q r
        t = findT q r 
        -- dis is the variable used to store the discriminant value
        dis = discriminant q r
        -- x1, x2 and x3 are calculated based on the values obtained from the cubic functions
        x1= s + t - (b/(3*a))
        x2= (-1*(s + t)/2) - (b/(3*a)) + (sqrt(3)/2)*(s - t)
        x3= (-1*(s + t)/2) - (b/(3*a)) - (sqrt(3)/2)*(s - t)

{- -----------------------------------------------------------------
 - findRoot
 - -----------------------------------------------------------------
 - Description: if x is negative it takes the ablsoute value since cube roots can't 
 - be applied to negative numbers in Haskell, if x is positive it simply cube roots the number
 -}
 findRoot :: Double -> Double
 findRoot x = 
    if x < 0 
        then (-1)*(abs x**(1/3))    
        else x**(1/3)
