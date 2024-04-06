import Data.Bifunctor

-- Recursive definition
data Tree = Nd Int Trees
data Trees = Nl | Cns Tree Trees

-- Mapping into a pair of types 
-- same as a pair of mappings (TreeF, TreesF)
data TreeF  t ts = Node Int ts 
data TreesF t ts = Nil | Cons t ts

-- Both are bifunctorial
instance Bifunctor TreeF where
    bimap :: (a -> b) -> (c -> d) -> TreeF a c -> TreeF b d
    bimap f g (Node n ts) = Node n (g ts)

instance Bifunctor TreesF where
    bimap f g Nil = Nil
    bimap f g (Cons t ts) = Cons (f t) (g ts)

-- The fixed point of a pair of bifunctors is a pair of types
newtype Mu1 f g = In1 { in1' :: f (Mu1 f g) (Mu2 f g)}
newtype Mu2 f g = In2 { in2' :: g (Mu1 f g) (Mu2 f g)}

-- smart constructors

node :: Int -> Mu2 TreeF TreesF -> Mu1 TreeF TreesF
node n ts = In1 (Node n ts)

nil :: Mu2 TreeF TreesF
nil = In2 Nil

cons :: Mu1 TreeF TreesF -> Mu2 TreeF TreesF -> Mu2 TreeF TreesF
cons t ts = In2 (Cons t ts)

-- 
tree1 :: Mu1 TreeF TreesF
tree1 = node 0 nil
tree2 :: Mu1 TreeF TreesF
tree2 = node 2 nil
trees3 :: Mu2 TreeF TreesF
trees3 = cons tree1 (cons tree2 trees3)
tree4 :: Mu1 TreeF TreesF
tree4 = node 3 trees3

-- traverse a rose tree
psi1 :: forall x y. 
    (x -> [Int]) -> (y -> [Int]) -> 
    (TreeF x y -> [Int], TreesF x y-> [Int])
psi1 ftree flist = (tree2lst, list2lst)
  where
    tree2lst (Node n l) = n : flist l
    list2lst Nil = []
    list2lst (Cons t ts) = ftree t ++ flist ts

-- prune subtrees with root 0
psi2 :: forall x y. 
    (x -> [Int]) -> (y -> [Int]) -> 
    (TreeF x y -> [Int], TreesF x y-> [Int])
psi2 ftree flist = (tree2lst, list2lst)
  where
    tree2lst (Node n l) = if n == 0 
                          then [0] -- prunning a branch
                          else n : flist l
    list2lst Nil = []
    list2lst (Cons t ts) = ftree t ++ flist ts

-- exit traversal when found 0
psi3 :: forall x y. 
    (x -> Maybe [Int]) -> (y -> [Int]) -> 
    (TreeF x y -> Maybe [Int], TreesF x y-> [Int])
psi3 ftree flist = (tree2lst, list2lst)
  where
    tree2lst (Node n l) = if n == 0 
                          then Nothing 
                          else Just (n : flist l)
    list2lst Nil = []
    list2lst (Cons t ts) = 
        case ftree t of
            Nothing -> [0] -- early exit
            Just tt -> tt ++ flist ts

-- An algebra in CxC is a pair (f a b -> a, g a b -> b)
-- A catamorphism produces a pair (Mu1 f g -> a, Mu2 f g -> b)
cata :: (forall x y. (x -> a) -> (y -> b) -> (f x y -> a, g x y -> b)) -> 
    (Mu1 f g -> a, Mu2 f g -> b)
cata psi = psi (cata1 psi) (cata2 psi) `bicompose` (in1', in2')

bicompose :: (b->c, b'->c') -> (a->b, a'->b') -> (a->c, a'->c')
bicompose (f, f') (g, g') = (f . g, f' . g')

-- Or split into two functions

cata1 :: (forall x y. (x -> a) -> (y -> b) -> (f x y -> a, g x y -> b)) -> 
    Mu1 f g -> a
cata1 psi = fst (psi (cata1 psi) (cata2 psi)) . in1'

cata2 :: (forall x y. (x -> a) -> (y -> b) -> (f x y -> a, g x y -> b)) -> 
    Mu2 f g -> b
cata2 psi = snd (psi (cata1 psi) (cata2 psi)) . in2'

-- produces infinite list
testInf :: [Int]
testInf = take 20 $ cata1 psi2 tree4

test2 = cata2 psi1 trees3

test3 :: Maybe [Int]
test3 = cata1 psi3 tree4