{-  This Module defines  parallel strategy combinators


	Phil Trinder 

	Version 7	1/5/96

The functions described here, and their use is documented in

"Strategies for writing parallel non-strict programs" Trinder
P.W. Loidl H.W. Peyton Jones S.L. in preparation.

N.B. This module may be rewritten to use constructor classes when they
become available in 1.3.

-}

module StrategiesVII(StrategiesVII.., Parallel..) where

-- import PreludeGlaST                        -- only needed for markStrat
import Parallel

{--------------------------------------------------------------------------------}
{-			Strategy Type, Application and Semantics	      	-}
{--------------------------------------------------------------------------------}

type Strategy a = a -> ()

{- A strategy takes a value and returns a unit value to indicate that
the specifed evaluation has been performed. -}

using :: a -> Strategy a -> a
using x s = s x `seq` x

{- using takes a strategy and a value, and applies the strategy to the
value before returning the value.

x `using` s is a projection on x, i.e. both

  a retraction: x `using` s [ x
			    -
  and idempotent: (x `using` s) `using` s = x `using` s
-}

{- sPar is a strategy corresponding to par. i.e. x `par` e <=> e `using` sPar x -}
sPar :: a -> Strategy b
sPar x y = x `par` ()


{- sSeq is a strategy corresponding to seq. i.e. x `seq` e <=> e `using` sSeq x -}
sSeq :: a -> Strategy b
sSeq x y = x `seq` ()

{--------------------------------------------------------------------------------}
{-			Basic Strategies				      	-}
{--------------------------------------------------------------------------------}

{- performs *no* evaluation on it's argument -}
r0 :: Strategy a 
r0 x = ()

{- reduces it's argument to weak head normal form -}
rwhnf :: Strategy a 
rwhnf x = x `seq` ()  

class NFData a where
{- rnf reduces it's argument to (head) normal form -}
  rnf :: Strategy a
{- Default method. Useful for base types. A specific method is necessay for
   constructed types -}
  rnf = rwhnf

class (NFData a, Integral a) => NFDataIntegral a
class (NFData a, Ord a) => NFDataOrd a

{-
------------------------------------------------------------------------------
			Marking a Strategy
------------------------------------------------------------------------------

Marking a strategy.

Actually, @markStrat@  sticks a label @n@  into the sparkname  field of the
thread executing strategy @s@. Together with a runtime-system that supports
propagation of sparknames to the children this means that this strategy and
all its children have  the sparkname @n@ (if the  static sparkname field in
the @parGlobal@ annotation contains the value 1). Note, that the @SN@ field
of starting the marked strategy itself contains the sparkname of the parent
thread. The END event contains @n@ as sparkname.

 markStrat :: Int -> Strategy a -> Strategy a 
 markStrat n s x = unsafePerformPrimIO (
     _casm_ ``%r = set_sparkname(CurrentTSO, %0);'' n `thenPrimIO` \ z ->
     returnPrimIO (s x))
-}

{--------------------------------------------------------------------------------}
{-			Strategy Instances and Functions		      	-}
{--------------------------------------------------------------------------------}

{-			Tuples						      	-}
{--------------------------------------------------------------------------------}

instance (NFData a, NFData b) => NFData (a,b) where
  rnf (x,y) = rnf x `seq` rnf y

instance (NFData a, NFData b, NFData c) => NFData (a,b,c) where
  rnf (x,y,z) = rnf x `seq` rnf y `seq` rnf z 

instance (NFData a, NFData b, NFData c, NFData d) => NFData (a,b,c,d) where
  rnf (x1,x2,x3,x4) = rnf x1 `seq` 
		      rnf x2 `seq` 
		      rnf x3 `seq` 
		      rnf x4 

seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
seqPair strata stratb (x,y) = strata x `seq` stratb y 

parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair strata stratb (x,y) = strata x `par` stratb y `par` ()

{- The reason for the  second `par` is so that the strategy terminates 
quickly. This is important if the strategy is used as the 1st argument of a seq -}

seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
seqTriple strata stratb stratc p@(x,y,z) = 
  strata x `seq` 
  stratb y `seq`
  stratc z 

parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
parTriple strata stratb stratc (x,y,z) = 
  strata x `par` 
  stratb y `par` 
  stratc z `par`
  ()

{-			Numbers						      	-}
{--------------------------------------------------------------------------------}
{- Weak head normal form and normal form are identical for integers, so the default 
rnf is sufficient. -}

instance NFData Int 

instance NFData Integer

instance NFDataIntegral Int
instance NFDataOrd Int

instance (NFData a) => NFData (Ratio a) where
  rnf (x:%y) = rnf x `seq` 
               rnf y `seq`
               ()

instance (NFData a) => NFData (Complex a) where
  rnf (x:+y) = rnf x `seq` 
	       rnf y `seq`
               ()


{-			Characters					      	-}
{--------------------------------------------------------------------------------}
instance NFData Char

{-			Unit						      	-}
{--------------------------------------------------------------------------------}

instance NFData ()

{-			Lists						      	-}
{--------------------------------------------------------------------------------}

instance NFData a => NFData [a] where
  rnf [] = ()
  rnf (x:xs) = rnf x `seq` rnf xs

{- Lists: Parallel Strategies
----------------------------}

{- Applies a strategy to every element of a list in parallel -}
parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)

{- Applies a strategy to the first  n elements of a list  in parallel -}
parListN :: (Integral b) => b -> Strategy a -> Strategy [a]
parListN n strat []     = ()
parListN 0 strat xs     = ()
parListN n strat (x:xs) = strat x `par` (parListN (n-1) strat xs)

{- Evaluates N elements of the spine of the argument list and applies
`strat' to the Nth element (if there is one) in parallel with the
result. e.g. parListNth 2 [e1, e2, e3] evaluates e2 -}
parListNth :: Int -> Strategy a -> Strategy [a]
parListNth n strat xs 
  | null rest = ()
  | otherwise = strat (head rest) `par` ()
  where
    rest = drop n xs


{- parListChunk sequentially applies a strategy to chunks
(sub-sequences) of a list in parallel. Useful to increase grain size-}
parListChunk :: Int -> Strategy a -> Strategy [a]
parListChunk n strat [] = ()
parListChunk n strat xs = seqListN n strat xs `par` 
			  parListChunk n strat (drop n xs)

{- parMap applies a function to each element of the argument list in
parallel.  The result of the function is evaluated using `strat' -}
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs 	= map f xs `using` parList strat

{- parFlatMap uses parMap to apply a list-valued function to each
element of the argument list in parallel.  The result of the function
is evaluated using `strat' -}
parFlatMap :: Strategy [b] -> (a -> [b]) -> [a] -> [b]
parFlatMap strat f xs = concat (parMap strat f xs)

{- parZipWith zips together two lists with a function z in parallel -}
parZipWith :: Strategy c -> (a -> b -> c) -> [a] -> [b] -> [c]
parZipWith strat z as bs = 
  zipWith z as bs `using` parList strat

{- Lists - Sequential Strategies
--------------------------------}

{- Sequentially applies a strategy to each element of a list -}

seqList :: Strategy a -> Strategy [a]
seqList strat []     = ()
seqList strat (x:xs) = strat x `seq` (seqList strat xs)


{- Sequentially applies a strategy to the first  n elements of a list -}

seqListN :: (Integral a) => a -> Strategy b -> Strategy [b]
seqListN n strat []     = ()
seqListN 0 strat xs     = ()
seqListN n strat (x:xs) = strat x `seq` (seqListN (n-1) strat xs)

{- seqListNth applies a strategy to the Nth element of it's argument
(if there is one) before returning the result. e.g. seqListNth 2 [e1,
e2, e3] evaluates e2 -}

seqListNth :: (Integral a) => a -> Strategy b -> Strategy [b]
seqListNth n strat xs 
  | null rest = ()
  | otherwise = strat (head rest) 
  where
    rest = drop n xs

{- fringeList implements a `rolling buffer' of length n, i.e.applies a
strategy to the nth element of list when the head is demanded. More
precisely:

   semantics:         fringeList n s = id :: [b] -> [b]
   dynamic behaviour: evalutates the nth element of the list when the
		      head is demanded.
   
The idea is to provide a `rolling buffer' of length n.
-}

fringeList :: (Integral a) => a -> Strategy b -> [b] -> [b]
fringeList n strat [] = []
fringeList n strat (r:rs) = 
  seqListNth n strat rs `par`
  r:fringeList n strat rs



