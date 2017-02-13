module RBTree

%access public export
%default total

data RBColor = Red | Black
Show RBColor where
  show Red = "R"
  show Black = "B"

data RBTree: Type -> Type where
     Branch: (Ord a) => RBColor -> RBTree a -> a -> RBTree a -> RBTree a
     EmptyLeaf: RBTree a


(Eq a) => Eq (RBTree a) where
  (==) EmptyLeaf EmptyLeaf = True
  (==) (Branch _ t1 z t2) (Branch _ t1' z' t2') = z == z' && t1 == t1' && t2 == t2'
  (==) _ _ = False

Show a => Show (RBTree a) where
 show (Branch c EmptyLeaf y EmptyLeaf) = "["++ show c ++ "," ++ show y ++"]"
 show (Branch c x y z) = "(" ++ (show x) ++ "," ++ "["++ show c ++ "," ++ show y ++"]" ++ "," ++ (show z) ++ ")"
 show EmptyLeaf = ""

length: RBTree a -> Int
length (Branch _ y _ w) = 1 + (length y) + (length w)
length EmptyLeaf = 0

balance: (Ord) a => RBTree a -> a -> RBTree a -> RBTree a
balance (Branch Red a x b) y (Branch Red c z d) = Branch Red (Branch Black a x b) y (Branch Black c z d)
balance (Branch Red (Branch Red a x b) y c) z d = Branch Red (Branch Black a x b) y (Branch Black c z d)
balance (Branch Red a x (Branch Red b y c)) z d = Branch Red (Branch Black a x b) y (Branch Black c z d)
balance a x (Branch Red b y (Branch Red c z d)) = Branch Red (Branch Black a x b) y (Branch Black c z d)
balance a x (Branch Red (Branch Red b y c) z d) = Branch Red (Branch Black a x b) y (Branch Black c z d)
balance a x b = Branch Black a x b

insert: (Ord a) => a -> %static RBTree a -> RBTree a
insert x s = case ins s of
                Branch _ a z b => Branch Black a z b
                EmptyLeaf => EmptyLeaf
             where
  ins: RBTree a -> RBTree a
  ins EmptyLeaf = Branch Red EmptyLeaf x EmptyLeaf
  ins s@(Branch Black a y b) = if x < y then
                                  balance (ins a) y b
                               else if x > y then
                                  balance a y (ins b)
                               else
                                  s
  ins s@(Branch Red a y b) =   if x < y then
                                  Branch Red (ins a) y b
                               else if x > y then
                                  Branch Red a y (ins b)
                               else s

contains: a -> RBTree a -> Bool
contains x EmptyLeaf = False
contains x (Branch _ a y b) = if x < y  then contains x a else if x > y then contains x b else True

fromList:(Ord a) => List a -> RBTree a
fromList [] = EmptyLeaf
fromList (x :: xs) = insert x (fromList xs)

toList: RBTree a -> List a
toList (Branch _ y z w) = (toList y) ++ (z :: (toList w))
toList EmptyLeaf = []

addAll:(Ord a) => List a -> RBTree a -> RBTree a
addAll [] x = x
addAll (y :: xs) x = addAll xs (insert y x)


(++): (Ord a) => RBTree a -> RBTree a -> RBTree a
(++) EmptyLeaf t = t
(++) t EmptyLeaf = t
(++) t1 t2 = addAll (toList t2) t1

map:(Ord b) => (a -> b) -> RBTree a -> RBTree b
map f EmptyLeaf = EmptyLeaf
map f (Branch _ t1 z t2) = (insert (f z) (map f t1)) ++ (map f t2)

(>>=):(Ord b) => RBTree a -> (a -> RBTree b) -> RBTree b
(>>=) EmptyLeaf f = EmptyLeaf
(>>=) (Branch _ t1 z t2) f = (f z) ++ (t1 >>= f) ++ (t2 >>= f)
