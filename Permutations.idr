module Permutations


import Data.Vect
import Streams

-- Find the largest index k such that a[k] < a[k + 1]. If no such index exists, the permutation is the last permutation.
step1: (Ord a) => Vect (S (S n)) a -> Maybe (Int, a)
step1 xs = step1' 0 Nothing xs where
  step1': (curIdx: Int) -> (curMaxIdx:Maybe (Int, a)) ->  Vect (S m) a -> Maybe (Int, a)
  step1' curIdx curMaxIdx (x :: [])  = curMaxIdx
  step1' curIdx curMaxIdx (x :: (y :: xs)) = if x < y then
                                                step1' (curIdx + 1) (Just (curIdx, x)) (y :: xs)
                                             else
                                                step1' (curIdx + 1) curMaxIdx (y :: xs)


-- Find the largest index l greater than k such that a[k] < a[l].

step2: (Ord a) => (Int, a) -> Vect (S n) a  -> Maybe (Int, a)
step2 p xs = step2' 0 Nothing p xs where
    step2': (curIdx: Int) -> (result: Maybe (Int, a)) -> (k: (Int, a)) -> Vect (S n) a -> Maybe (Int, a)
    step2' curIdx ml (k, ak) (x :: []) = if ak < x then (Just (curIdx, x))  else ml
    step2' curIdx ml (k, ak) (x :: (y :: xs)) = if curIdx > k then
                                           if ak < x then
                                              step2' (curIdx + 1) (Just (curIdx, x)) (k, ak) (y :: xs)
                                           else
                                              step2' (curIdx + 1) ml (k, ak) (y :: xs)
                                         else
                                           step2' (curIdx + 1) ml (k, ak) (y :: xs)

-- Swap the value of a[k] with that of a[l].

step3: (k:(Int, a)) -> (l:(Int, a)) -> Vect n a -> Vect n a
step3 k l xs = step3' 0 k l xs where
  step3': Int -> (k:(Int, a)) -> (l:(Int, a)) -> Vect n a -> Vect n a
  step3' curIndx k l [] = []
  step3' curIndx (k, ak) (l, al) (x :: xs) = if curIndx > l then
                                    (x :: xs)
                                 else if curIndx == l then
                                    ak :: step3' (curIndx + 1) (k, ak) (l, al) xs
                                 else if curIndx == k then
                                    al :: step3' (curIndx + 1) (k, ak) (l, al) xs
                                 else
                                    x :: step3' (curIndx + 1) (k, ak) (l, al) xs

-- Reverse the sequence from a[k + 1] up to and including the final element a[n].
step4: (k: Int) -> Vect (S n) a -> Vect (S n) a
step4 k xs = step4' 0 k xs where
  step4': Int -> Int -> Vect n a -> Vect n a
  step4' curIdx k (x :: xs) = if curIdx <= k then
                                  x :: step4' (curIdx + 1) k xs
                              else
                                  reverse (x :: xs)

nextPermutation:(Ord a) => Vect (S (S n)) a -> Maybe (Vect (S (S n)) a)
nextPermutation xs = do k <- step1 xs
                        l <- step2 k xs
                        Just (step4 (fst k) (step3 k l xs))

permutations: (Ord a) => Vect (S (S n)) a -> Stream (Vect (S (S n)) a)
permutations xs = xs :: case nextPermutation xs of
                              Just ys => permutations ys
                              Nothing => permutations xs
