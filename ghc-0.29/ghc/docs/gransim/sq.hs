-- Time-stamp: <Tue Jul 23 1996 15:48:21 Stardate: [-31]7853.08 hwloidl>
--
-- Example program for the GranSim User's Guide:  Sum-of-squares
--
-- Usage: sq <version> <list-length> 
--
-- This program shows a varying amount of parallelism because of a varying 
-- degree of evaluation.
-- Three versions of the program are shown:
--  1) A naive version that has almost no parallelism at all (the producer
--     only creates the top cons cell; all the rest is computed by the consumer
--     who is demanding the list)
--  2) A forcing function can be used to force the intermediate list. The
--     expression that is sparked is the application of the forcing function
--     to the data structure of interest.
--  3) A strategy version that creates the same behaviour but neatly separates
--     the definition of the result from the parallelism and evaluation degree.
--
-- Use GranSim-Light to run the problem and gr2ap to visualise the result.
--
-----------------------------------------------------------------------------

module Main where

--import Parallel
import StrategiesVII

main = getArgs abort ( \ a -> 
       let 
         args :: [Int]
         args = map (\ a1 -> fst ((readDec a1) !! 0)) a       
         version =  args !! 0      
         n = args !! 1
         -- l = [1..]
         squares :: [Int] 
         squares = [ i^2 | i <- [1..n] ]
         -- Naive version sparks a producer for list squares but doesn't force it
         res_naive = squares `par` sum squares
         -- This version sparks a forcing function on the list (producer)
         res_force = (foldl1 seq squares) `par` sum squares
         -- The strategy version
         res_strat = sum squares `using` (rnf squares `par` rnf)
         res = case version of
                1 -> res_naive		     
                2 -> res_force		     
                3 -> res_strat		     
                _ -> res_naive
         str = case version of
                1 -> "Naive version"
                2 -> "Forcing version"		     
                3 -> "Strategy version"
                _ -> "Naive version"
       in 
       print ("Sum of squares (" ++ str ++ ") of length " ++ (show n) ++ 
              " = " ++ (show res) ++ "\n")  )
       
