{--
 - A sequential version of a simple Monte Carlo PI estimator
 - compile: ghc --make -rtsopts -O2 mc-pi.hs
 - run: ./mc-pi <NSAMPLES> [<SEED>]
 - 
 - eb120@hw.ac.uk init: 23-01-2015
 - TODO: make seed optional
 -       introduce outer loop NITER?
 -       add haskell-level timing (s + ms) and make output conform to the C and pthreads output style (for script re-use)
 --}

module Main(main) where

import System.Environment(getArgs)
import System.Random(StdGen,mkStdGen,randomR)
import Text.Printf(printf)

main :: IO ()
main = do 
  args <- getArgs
  if null args || (length args) < 2 
    then error "usage: please supply two arguments (number of samples and the seed)\n"
    else do
      let
        nsamples = read (args!!0)  -- number of samples (problem size) vs [l] = map read `fmap` getArgs or similar
        seed     = read (args!!1)
        res      = mcPi nsamples seed
      putStrLn ("sequential MC pi: NSAMPLES=" ++ (show nsamples) ++ " and SEED " ++ (show seed) ++ " = " ++ (show res))
      printf "error: %.6f\n" (res-pi)


mcPi :: Integer -> Int -> Double
mcPi nsamples seed = go prng (0,nsamples)
     where
       prng = mkStdGen seed                                                      -- mkStdGen :: Int -> StdGen
       go :: (Num a, Integral a) => StdGen -> (a,a) -> Double
       go  _  (hits,0)  =  4.0 * (fromIntegral hits) / (fromIntegral nsamples)   
       go gen (hits,n)  =  go newGen' (hits+h,n-1) 
         where
           range = ((-1.0), 1.0)
           (x, newGen)  = randomR range gen    :: (Double, StdGen)
           (y, newGen') = randomR range newGen :: (Double, StdGen)
           h | (x*x + y*y) <= 1.0  = 1
             | otherwise           = 0

-- TODO: add timing output (fix output to be conformant)
--       compare + optimise (e.g. UNPACK etc), use ByteString, BangPatterns

-- random  :: (RandomGen g, Random a) => g -> (a, g)
-- randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)

-- take n $ randoms gen :: [Double]
-- take n $ randomRs (0,100) gen :: [Double]



