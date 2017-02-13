module Streams

%access export

numberStream:(Num a) => a -> Stream a
numberStream x = x :: (numberStream (x + 1))

multiplesOf: (m: Integer) -> Stream Integer
multiplesOf m = map (* m) (numberStream 1)


filter:(a -> Bool) -> Stream a -> Stream a
filter f (value :: xs) = if f value then (value :: (filter f xs)) else filter f xs

foldUntil: (continue: elem -> Bool) -> (acc -> elem -> acc) -> (init: acc) -> Stream elem -> acc
foldUntil continue f init (value :: xs) = if continue value then
                                             foldUntil continue f (f init value) xs
                                           else
                                              init

sum: (Num a) => (continue: a -> Bool) -> Stream a -> a
sum continue = foldUntil continue (+) 0

sumUntil: (Num a, Ord a) => a -> Stream a -> a
sumUntil max = sum (< max)

combine: Stream Integer -> Stream Integer -> Stream Integer
combine (value :: xs) (value2 :: ys) =
      if value < value2 then
         value :: combine xs (value2 :: ys)
      else if value > value2 then
         value2 :: combine (value :: xs) ys
      else
         value :: combine xs ys
