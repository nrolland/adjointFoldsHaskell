module MutuCata where

newtype Mu f = In { in' :: f (Mu f) }

data NatF x = Z | S x
  deriving Functor

toInt :: Mu NatF -> Int
toInt (In Z) = 0
toInt (In (S x)) = toInt x + 1

fromInt :: Int -> Mu NatF
fromInt 0 = In Z
fromInt n = In $ S $ fromInt (n - 1)

-- L = Delta, R = (,)
-- Psi :: forall x. (L x -> a) -> (L (F x) -> a)
-- cataM :: (forall x. (L x -> a) -> (L (F x) -> a)) -> (L(Mu F) -> a)

-- psi :: (x -> a, x -> b) -> (F x -> a, F x -> b)
 
cataM1 :: (forall x. (x -> a, x -> b) -> (f x -> a, f x -> b)) -> 
          Mu f -> a
cataM1 psi = fst (psi (cataM1 psi, cataM2 psi)) . in'
cataM2 :: (forall x. (x -> a, x -> b) -> (f x -> a, f x -> b)) -> Mu f -> b
cataM2 psi = snd (psi (cataM1 psi, cataM2 psi)) . in'

psi :: (x -> Int, x -> Int) -> (NatF x -> Int, NatF x -> Int)
psi (fac, idd) = (f, g)
  where
    f Z = 1
    f (S n) = (idd n + 1) * fac n
    g Z = 0
    g (S n) = idd n + 1

test = cataM1 psi (fromInt 4)
