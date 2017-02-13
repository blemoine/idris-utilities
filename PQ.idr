module PQ

%access export

data Set a = Bin Int a (Set a) (Set a) | Tip

empty: Set a
empty = Tip

singleton: a -> Set a
singleton x = Bin 1 x Tip Tip

size: Set a -> Int
size Tip = 0
size (Bin sz _ _ _) = sz

ratio: Int
ratio = 2

delta: Int
delta = 3


balanceR: a -> Set a -> Set a -> Set a
balanceR x l r = case l of
  Tip => case r of
           Tip => Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) => Bin 2 x Tip r
           (Bin _ rx Tip rr@(Bin _ _ _ _)) => Bin 3 rx (Bin 1 x Tip Tip) rr
           (Bin _ rx (Bin _ rlx _ _) Tip) => Bin 3 rlx (Bin 1 x Tip Tip) (Bin 1 rx Tip Tip)
           (Bin rs rx rl@(Bin rls rlx rll rlr) rr@(Bin rrs _ _ _)) =>
             if rls < ratio*rrs then Bin (1+rs) rx (Bin (1+rls) x Tip rl) rr
             else Bin (1+rs) rlx (Bin (1+size rll) x Tip rll) (Bin (1+rrs+size rlr) rx rlr rr)

  (Bin ls _ _ _) => case r of
           Tip => Bin (1+ls) x l Tip

           (Bin rs rx rl rr) =>
              if rs > delta*ls  then case (rl, rr) of
                   (Bin rls rlx rll rlr, Bin rrs _ _ _) =>
                     if rls < ratio*rrs then Bin (1+ls+rs) rx (Bin (1+ls+rls) x l rl) rr
                     else Bin (1+ls+rs) rlx (Bin (1+ls+size rll) x l rll) (Bin (1+rrs+size rlr) rx rlr rr)
              else Bin (1+ls+rs) x l r

balanceL: a -> Set a -> Set a -> Set a
balanceL x l r = case r of
  Tip => case l of
           Tip => Bin 1 x Tip Tip
           (Bin _ _ Tip Tip) => Bin 2 x l Tip
           (Bin _ lx Tip (Bin _ lrx _ _)) => Bin 3 lrx (Bin 1 lx Tip Tip) (Bin 1 x Tip Tip)
           (Bin _ lx ll@(Bin _ _ _ _) Tip) => Bin 3 lx ll (Bin 1 x Tip Tip)
           (Bin ls lx ll@(Bin lls _ _ _) lr@(Bin lrs lrx lrl lrr)) =>
             if lrs < ratio*lls then Bin (1+ls) lx ll (Bin (1+lrs) x lr Tip)
             else Bin (1+ls) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+size lrr) x lrr Tip)

  (Bin rs _ _ _) => case l of
           Tip => Bin (1+rs) x Tip r

           (Bin ls lx ll lr) =>
              if ls > delta*rs  then case (ll, lr) of
                   (Bin lls _ _ _, Bin lrs lrx lrl lrr) =>
                     if lrs < ratio*lls then Bin (1+ls+rs) lx ll (Bin (1+rs+lrs) x lr r)
                     else Bin (1+ls+rs) lrx (Bin (1+lls+size lrl) lx ll lrl) (Bin (1+rs+size lrr) x lrr r)

              else Bin (1+ls+rs) x l r

minViewSure: a -> Set a -> Set a -> (a, Set a)
minViewSure = go
  where
    go x Tip r = (x, r)
    go x (Bin _ xl ll lr) r =
      case go xl ll lr of
        (xm, l') => (xm, balanceR x l' r)

minView: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView (Bin _ x l r) = Just  (minViewSure x l r)

total
ptrEq: (Eq a) => Set a -> Set a -> Bool
ptrEq Tip Tip = True
ptrEq (Bin s a xs ys) (Bin s2 a2 xs2 ys2) = (s == s2) && (a == a2) && ptrEq xs xs2 && ptrEq ys ys2
ptrEq _ _ = False

insert: Ord a => a -> Set a -> Set a
insert = go
  where
    go: Ord a => a -> Set a -> Set a
    go x Tip = singleton x
    go x t@(Bin sz y l r) = case compare x y of
        LT => let l' = go x l in
                if l' `ptrEq` l then t
                else balanceL y l' r

        GT => let r' = go x r in
                if r' `ptrEq` r then t
                else balanceR y l r'

        EQ => if x == y then t
              else Bin sz x l r


lookupMinSure: a -> Set a -> a
lookupMinSure x Tip = x
lookupMinSure _ (Bin _ x l _) = lookupMinSure x l

lookupMin: Set a -> Maybe a
lookupMin Tip = Nothing
lookupMin (Bin _ x l _) = Just $ lookupMinSure x l

findMin: Set a -> a
findMin t = case lookupMin t of
    Just r => r
