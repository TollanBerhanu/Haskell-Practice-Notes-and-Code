-- System.Random module has all the functions that satisfy our need for randomness like 'random'
--  random :: (RandomGen g, Random a) => g -> (a, g) ... RandomGen typeclass is for types that can act as sources of
--  randomness (StdGen). The Random typeclass is for things that can take on random values (Int, Bool, Float)

-- stdGen is a random number generator. We can use mkStdGen and pass an Int to give us a random generator 
{- import System.Random
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  -} 

import System.Random  
  
main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)  