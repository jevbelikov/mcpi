{--
 - A parallel version of a simple Monte Carlo PI estimator
 - compile: ghc --make -rtsopts -O2 -threaded -eventlog mc-pi-smp.hs
 -     or   ghc --make -rtsopts -O2 -oparpvm mc-pi-gum.hs
 - run: ./mc-pi-smp <NSAMPLES> [<SEED>] +RTS -N<n> -sstderr
 -  or  ./mc-pi-gum <NSAMPLES> [<SEED>] +RTS -qp<n> -qPg
 -
 - NB: add -DNEED_PARMAP compiler flag to use the below parMap implementation
 -
 - eb120@hw.ac.uk init: 23-01-2015
 - TODO: make seed optional
 -       introduce outer loop NITER?
 -       add haskell-level timing (s + ms) and make output conform to the C adn pthreads output style (for script re-use)
 -
 - GHCSMP:  ghc --make -threaded -eventlog -rtsopts -O2 -o mc-pi-thr-2 mc-pi-parmap.hs
 - GUM: $  <path-to-GUM-compiler>/ghc-stage1 --make -cpp -fforce-recomp -fglasgow-exts -O2 -i<path-to-strategy-libs> -rtsopts -parpvm -DNEED_PARMAP -hisuf="pp_P2_hi" -osuf="pp_P2_O2_o" -o mc_pi_O2_pp mc-pi-parmap.hs 
 --}

{-# LANGUAGE CPP #-}

module Main(main) where

import System.Environment(getArgs)
import System.Random(StdGen,mkStdGen,randomR,randoms)
import Text.Printf(printf)

import Control.Parallel
import Control.Parallel.Strategies

#ifdef NEED_PARMAP
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f = (`using` parList strat) . map f
#endif 

main = do 
  args <- getArgs
  if null args || (length args) < 3 
    then error "usage: please supply three arguments (number of samples, chunk size and the seed)\n"
    else do
      let
        nsamples = read (args!!0)  :: Integer -- number of samples (problem size) vs [l] = map read `fmap` getArgs or similar
        nchunks  = read (args!!1)  :: Int     -- chunk number
        chunk_sz = nsamples `div` (toInteger nchunks)     -- TODO: for now ensure nsamples cleanly divides by chunks
        seed     = read (args!!2)  :: Int     -- seed
        gen      = mkStdGen seed
        seeds    = take nchunks $ randoms gen  -- create a seed for each prng	
        intermediates = parMap rdeepseq (mcPiI chunk_sz) seeds
        res      = 4.0 * (fromIntegral $ sum intermediates) / (fromIntegral nsamples) :: Double 
                   --sum of intermediate results divided by number of samples (each is chunk is sparked using parMap))  
      putStrLn ("parmap MC pi: NSAMPLES=" ++ (show nsamples) ++ " and SEED " ++ (show seed) ++ " = " ++ (show res))
      printf "error: %.6f\n" (res-pi)

-- create nPEs chunks of size smaples/nPEs (last chunk slightly larger +mod)

-- returns the number of hits (for the chunk)
-- caller sums all up and divides once (to avoid floating point arithmetic)
mcPiI :: Integer -> Int -> Integer
mcPiI nsamples seed = go prng (0,nsamples)
     where
       prng = mkStdGen seed                                                      -- mkStdGen :: Int -> StdGen
       go  _  (hits,0)  =  hits
       go gen (hits,n)  =  go newGen' (hits+h,n-1) 
         where
           range = ((-1.0), 1.0)
           (x, newGen)  = randomR range gen    :: (Double, StdGen)
           (y, newGen') = randomR range newGen :: (Double, StdGen)
           h | (x*x + y*y) <= 1.0  = 1
             | otherwise           = 0


mcPi :: Integer -> Int -> Double
mcPi nsamples seed = go prng (0,nsamples)
     where
       prng = mkStdGen seed                                                      -- mkStdGen :: Int -> StdGen
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



