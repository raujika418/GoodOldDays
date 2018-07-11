-- Time-stamp: <Tue Jul 23 1996 15:53:09 Stardate: [-31]7853.10 hwloidl>
--
-- Example program for the GranSim User's Guide:  Quicksort
--
-- Usage: qs <version> <list-length> 
--
-- Three versions of quicksort as discussed in the GranSim User's Guide
-- and in the Strategies paper.
--  1) A naive version that has almost no parallelism because the sub-lists
--     are only evaluated to WHNF.
--  2) A forcing function can be used to force the evaluation of both
--     sublists in each recursive call.
--  3) A strategy version that creates the same behaviour but neatly separates
--     the definition of the result from the parallelism and evaluation degree.
--
-- Use GranSim-Light to run the problem and gr2ap to visualise the result.
-- Use a list length of around 50 to get good graphs.
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
         l = [n,n-1..1]

         -- Naive version 
         resN = quicksortN l
         -- Version with a forcing function
         resF = quicksortF l
         -- The strategy version
         resS = quicksortS l

         res = case version of
                1 -> resN
                2 -> resF
                3 -> resS
                _ -> resN
         str = case version of
                1 -> "Naive version"
                2 -> "Forcing version"		     
                3 -> "Strategy version"
                _ -> "Naive version"
       in 
       print (str ++ " \n" ++ 
              "Input list (length " ++ (show (length l)) ++ ") is " ++ 
              (if is_sorted l then "sorted.\n" else "NOT sorted.\n") ++
	      "Output list (length " ++ (show (length res)) ++ ") is " ++ 
              (if is_sorted res then "sorted.\n" else "NOT sorted.\n")))

-- Naive version with poor parallelism
quicksortN :: [Int] -> [Int]
quicksortN []     = []
quicksortN [x]    = [x]
quicksortN (x:xs) = 
 losort `par`
 hisort `par`
 result        
  where    
   losort = quicksortN [y|y <- xs, y < x] 
   hisort = quicksortN [y|y <- xs, y >= x]
   result = losort ++ (x:hisort)

-- Version using a forcing function to obtain better parallelism
quicksortF :: [Int] -> [Int]
quicksortF []      = []
quicksortF [x]     = [x]
quicksortF (x:xs)  = 
 (forceList losort) `par`
 (forceList hisort) `par`
 losort ++ (x:hisort)
  where
   losort = quicksortF [y|y <- xs, y < x] 
   hisort = quicksortF [y|y <- xs, y >= x]

forceList :: [a] -> ()
forceList [] = ()
forceList (x:xs) = x `seq` forceList xs

-- Strategy version
quicksortS :: [Int] -> [Int]
quicksortS []      = []
quicksortS (x:xs)  = losort ++ (x:hisort) `using` strategy 
		    where
		      losort = quicksortS [y|y <- xs, y < x] 
		      hisort = quicksortS [y|y <- xs, y >= x]
		      strategy result = 
			rnf losort `par`
		    	rnf hisort `par`
		    	rnf result `seq`
                        ()

-- Only used for checking the result
is_sorted :: [Int] -> Bool
is_sorted l = and (zipWith (<) l (tail l))
       
