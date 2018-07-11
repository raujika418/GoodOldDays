%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[Parallel]{Parallel Constructs}

\begin{code}
module Parallel (par, seq
#if /* defined(__CONCURRENT_HASKELL__) && */ defined(__GRANSIM__)
                 , parGlobal, parLocal, parAt, parAtForNow     
#endif
                ) 
where

infixr 0 `par`
infixr 1 `seq`

par, seq :: a -> b -> b

#if /* defined(__CONCURRENT_HASKELL__) && */ defined(__PARALLEL_HASKELL__)

-- Just names without the ugly underscores

{-# INLINE par #-}
par a b = _par_ a b

{-# INLINE seq #-}
seq a b = _seq_ a b

#elif /* defined(__CONCURRENT_HASKELL__) && */ defined(__GRANSIM__)

{-# INLINE parGlobal #-}
parGlobal :: Int -> Int -> Int -> Int -> a -> b -> b
parGlobal (I# n) (I# g) (I# s) (I# p) = _parGlobal_ n g s p


parLocal :: Int -> Int -> Int -> Int -> a -> b -> b
parLocal (I# n) (I# g) (I# s) (I# p) = _parLocal_ n g s p


parAt :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAt (I# n) (I# g) (I# s) (I# p) = _parAt_ n g s p


parAtForNow :: Int -> Int -> Int -> Int -> a -> b -> c -> c
parAtForNow (I# n) (I# g) (I# s) (I# p) = _parAtForNow_ n g s p

{-# INLINE seq #-}
seq a b = _seq_ a b

{-# INLINE par #-}
par a b = _parGlobal_ 1# 1# 0# 0# a b

#else

par a b = b
seq a b = b

-- Maybe parIO and the like could be added here later.

#endif {- __CONCURRENT_HASKELL__ -}
\end{code}

