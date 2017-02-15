module Numeric

divMod: (Integral a) => a -> a -> (a, a)
divMod n d= (n `div` d, n `mod` d)

seq: a -> b -> b
seq _ b = b

{-- Support only base between 1 and 10 --}
showNatAtBase: Nat -> (base:Nat) -> String
showNatAtBase n0 base = pack $ showIt (divMod n0 base) [] where
      toChr: Nat -> List Char
      toChr x = if x < 10 then unpack $ show x else ['?']
      showIt: (Nat, Nat) -> List Char -> List Char
      showIt (n, d) r = let c: List Char = toChr d in
                        let r': List Char = c ++ r in
                            case n of
                                  Z => r'
                                  (S k) => showIt (divMod n base) r'
