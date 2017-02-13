module Primes

import PQ

%access export

Eq (Stream Integer) where
  (==) (x :: xs) (y :: ys) = x == y

Ord (Stream Integer) where
  compare (x :: xs) (y :: ys) = compare x y

oddStream: Integer -> Stream Integer
oddStream s = s :: (oddStream (s + 2))


primes: Stream Integer
primes = 2 :: sieve (oddStream 3) where

    sieve: Stream Integer -> Stream Integer
    sieve (x :: xs) = x :: sieve' xs (insertPrime x xs PQ.empty) where

      insertPrime: Integer -> Stream Integer -> PQ.Set (Integer, Stream Integer) -> PQ.Set (Integer, Stream Integer)
      insertPrime p xs = PQ.insert (p*p, map (* p) xs)

      sieve': Stream Integer -> PQ.Set (Integer, Stream Integer) -> Stream Integer
      sieve' (x :: xs) table = if fst (PQ.findMin table) == x then sieve' xs (adjust x table)
                               else x :: sieve' xs (insertPrime x xs table)
                               where


        adjust: Integer ->  PQ.Set (Integer, Stream Integer) -> PQ.Set (Integer, Stream Integer)
        adjust x table = case PQ.minView table of
              Just ((n, n' :: ns), newPQ) => if n == x then adjust x (PQ.insert (n', ns) newPQ)
                                             else table
              Nothing => table



primeFactors: Integer -> List Integer
primeFactors n = factors n primes
 where
  factors:Integer -> Stream Integer -> List Integer
  factors 1 _                  = []
  factors m (p::ps) = if m < p*p   then
                        [m]
                      else if (m `mod` p) == 0    then
                        p :: factors (m `div` p) (p::ps)
                      else
                        factors m ps


isPrime: Integer -> Bool
isPrime n = primeFactors n == [n]
