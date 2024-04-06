

newtype Fix f = Fix { unFix :: f (Fix f) }

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coa = Fix . fmap (ana coa) . coa

hylo :: Functor f => (a -> f a) -> (f b -> b) -> a -> b
hylo coalg alg = cata alg . ana coalg

meta :: Functor f => (f a -> a) -> (a -> b) -> (b -> f b) -> Fix f -> Fix f
meta alg h coalg = ana coalg . h . cata alg

cataM :: (forall x . (x -> a) -> f x -> a) -> Fix f -> a
cataM psi = psi (cataM psi) . unFix

anaM :: (forall x. (a -> x) -> a -> f x) -> a -> Fix f
anaM phi = Fix . phi (anaM phi)

hyloM :: (forall c. (a -> c) -> (a -> f c)) -> 
         (forall c. (c -> b) -> (f c -> b)) -> 
         a -> b 
hyloM phi psi = cataM psi . anaM phi

metaM :: (forall c. (c -> b) -> (f c -> b)) -> 
         (b -> a) -> 
         (forall c. (a -> c) -> (a -> f c)) -> 
         Fix f -> Fix f
metaM psi h phi = anaM phi . h . cataM psi

fromAlg :: Functor f => (f a -> a) -> (forall x . (x -> a) -> f x -> a)
fromAlg alg h = alg . fmap h

fromCoalg :: Functor f => (a -> f a) -> (forall x. (a -> x) -> a -> f x)
fromCoalg coa h = fmap h . coa

data ListF a x = Nil | Cons a x
  deriving Functor

nil :: Fix (ListF a)
nil = Fix Nil

cons :: a -> Fix (ListF a) -> Fix (ListF a)
cons a l = Fix (Cons a l)

fromList :: [a] -> Fix (ListF a)
fromList = foldr cons nil

toList :: Fix (ListF a) -> [a]
toList = cata toListAlg

toListAlg :: ListF a [a] -> [a]
toListAlg Nil = []
toListAlg (Cons a x) = a : x

psi :: forall x . (x -> [Int]) -> ListF Int x -> [Int]
psi k Nil = []
psi k (Cons a x) = 
        if a == 0 
        then []
        else a : k x

-- Coalgebra generating a list of decreasing integers
seqn :: Int -> ListF Int Int
seqn k = Cons k (k - 1)

test1 = take 10 $ toList $ ana seqn 3

-- Early exit on zero
test2 = hyloM (fromCoalg seqn) psi 5