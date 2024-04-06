module CurryFold where

-- Currying adjunction
-- Append one list to another

newtype Mu f = In { in' :: f (Mu f) }
newtype Nu f = Out { out' :: f (Nu f) }

data ListF a x = Nil | Cons a x
  deriving Functor

nil :: Mu (ListF a)
nil = In Nil

cons :: a -> Mu (ListF a) -> Mu (ListF a)
cons a l = In (Cons a l)

fromList :: [a] -> Mu (ListF a)
fromList = foldr cons nil

shunt :: (Mu (ListF a), [a]) -> [a]
shunt (In Nil, as) = as
shunt (In (Cons a x), as) = shunt (x, a : as)

test = shunt (fromList [1, 2, 3], [10, 20, 30])

-- algebra L (f (R a)) -> a
-- Yoneda with adjunction
-- psi :: forall x. (L x -> a) -> (L (f x) -> a)
-- cataM :: (forall x. (L x -> a) -> (L (f x) -> a)) -> L (Mu f) -> a
-- cataM . L In = psi cata

data L a x = L a x
  deriving Functor

data R a x = R (a -> x)
  deriving Functor

-- F (mu F) - F cata -> F (R a)    (L.F.R) a
--  |                     |           |
--  v                     v           v
-- mu F  ----- cata -->  R a          a
--
-- L (mu F) --       ->   a


-- (x -> R b) -> (L (f x) -> b)
-- L (f (R b)) -> b
-- psi :: (forall x. (L x -> b) -> (L (F x) -> b))
-- cata :: (forall x. (L x -> b) -> (L (F x) -> b)) -> L (Mu f) -> b

-- F = ListF a, L = (-, b), R = b -> -, b = [a]
-- psi :: (forall x. (L x -> b) -> (L (F x) -> b))
-- cata :: (forall x. (L x -> b) -> (L (F x) -> b)) -> L (Mu f) -> b
cataM :: Functor l => (forall x. (l x -> b) -> (l (f x) -> b)) -> l (Mu f) -> b
cataM psi = psi (cataM psi) . fmap in' 

psi :: (forall x. (([a], x) -> [a]) -> ([a], ListF a x) -> [a])
psi h (as, Nil) = as
psi h (as, Cons a x) = h (a : as, x)

test2 = cataM psi ([10, 20, 30], fromList [1, 2, 3])

-- anaM phi :: a -> r (Nu f)
-- phi (anaM phi) :: a -> r (f (Nu f))
anaM :: Functor r => (forall x. (a -> r x) -> a -> r (f x)) -> 
        a -> r (Nu f)
anaM phi = fmap Out . phi (anaM phi)

----

cata :: Functor f => (f a -> a) -> (Mu f -> a)
cata alg = alg . fmap (cata alg) . in'


{-
F (mu F) - F cata -> F (R a)    (L.F.R) a
  |                     |           |
  v                     v           v
 mu F  ----- cata -->  R a          a

 L (mu F) -- cataT ->   a
-}
class (Functor l, Functor r) => Adjunction l r where
  ltor :: (l x -> y) -> (x -> r y)
  rtol :: (x -> r y) -> (l x -> y)

-- Algebra transformer
cataT :: (Adjunction l r, Functor f) => (l(f(r a)) -> a) -> (l (Mu f) -> a)
cataT alg = rtol (cata (ltor alg)) 

instance Adjunction ((,) e) ((->) e) where
  ltor f a e      = f (e, a)
  rtol f (e, a) = f a e

-- algebra : l(f(r x)) -> x
-- (e, f (e -> x)) -> x
-- f (e -> x) -> e -> x
-- substitute e = [a], x = [a], f = ListF a
alg :: ([a], ListF a ([a] -> [a])) -> [a]
alg (as, Nil) = as
alg (as, Cons a f) = f (a : as)

testT = cataT alg ([10, 20, 30], fromList [1, 2, 3])
