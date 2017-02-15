module Numeric

%access export

divMod: (Integral a) => a -> a -> (a, a)
divMod n d= (n `div` d, n `mod` d)

seq: a -> b -> b
seq _ b = b

{-- Support only base between 1 and 10 --}
showIntAtBase: Integer -> (base:Nat) -> String
showIntAtBase n0 base = let bInt = (cast base) in pack $ showIt bInt (divMod n0 bInt) [] where
      toChr: Integer -> List Char
      toChr x = if x < 10 then unpack $ show x else ['?']
      showIt: (base:Integer) -> (Integer, Integer) -> List Char -> List Char
      showIt base (n, d) r = let r' = (toChr d ) ++ r in
                                if n == 0 then
                                    r'
                                else
                                    showIt base (divMod n  base) r'
