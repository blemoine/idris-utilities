module Cache

import RBTree

%access public export
%default total


data CachePair a b = MkCachePair a b

(Show a, Show b) => Show (CachePair a b) where
  show (MkCachePair a b) = (show a) ++ (show b)

(Eq a) => Eq (CachePair a b) where
   (==) (MkCachePair a b) (MkCachePair a' b') = a == a'

(Ord a) => Ord (CachePair a b) where
   compare (MkCachePair a b) (MkCachePair a' b') = compare a a'


Cache:Type -> Type -> Type
Cache a b = RBTree (CachePair a b)

data Cached:Type -> Type -> Type where
    MkCached: Cache a b -> b -> Cached a b

getValue: Cached a b -> b
getValue (MkCached c b) = b

(Show a, Show b) => Show (Cached a b) where
    show (MkCached c b) = (show c) ++ ";" ++ (show b)

putAndGet: (Ord a) => (a -> b) -> a -> Cache a b -> Cached a b
putAndGet f a c = let b = f a in MkCached (insert (MkCachePair a b) c) b

find: (a -> Ordering) -> Cache a b -> Maybe b
find f EmptyLeaf = Nothing
find f (Branch _ t1 (MkCachePair a b) t2) = case f a of
                              LT => find f t1
                              GT => find f t2
                              EQ => Just b

compute: (Ord a) =>  (a -> b) -> (Cache a b) -> a -> Cached a b
compute f cache a = case find (compare a) cache of
                          Just b => MkCached cache b
                          Nothing => putAndGet f a cache

memoize: (Ord a) => (a -> b) -> a -> Cached a b
memoize f a = compute f EmptyLeaf a

flatCompute:  (Ord a) => (a -> (Cache a b) -> Cached a b) -> a -> (Cache a b) -> Cached a b
flatCompute f a cache = case find (compare a) cache of
                            Just b => MkCached cache b
                            Nothing => case f a cache of
                                (MkCached c b) => MkCached (insert (MkCachePair a b) c) b


map: (Ord a) => (b -> c) -> Cache a b -> Cache a c
map f EmptyLeaf = EmptyLeaf
map f (Branch c t1 (MkCachePair x y) t2) = Branch c (map f t1) (MkCachePair x (f y)) (map f t2)


{--
fib: Integer -> Cached Integer Integer
fib n = fib' n EmptyLeaf where
    fib': Integer -> Cache Integer Integer -> Cached Integer Integer
    fib' 0 c = MkCached c 1
    fib' 1 c = MkCached c 1
    fib' i c =  case flatCompute fib' (i - 1) c of
                    (MkCached cache val) => case flatCompute fib' (i - 2) cache of
                        (MkCached newCache val2) => MkCached newCache (val + val2)
--}
