module MutuAna where

newtype Nu f = Out { out' :: f (Nu f) }

data SeqF x = NextF Int x
  deriving Functor

toList :: Nu SeqF -> [Int]
toList (Out (NextF n x)) = n : toList x

nats :: Int -> Nu SeqF
nats n = Out (NextF n (squares n))

squares :: Int -> Nu SeqF
squares n = Out (NextF (n * n) (nats (n + 1)))

test = take 10 $ toList $ nats 3

-- R = Delta
phi1, phi2 :: forall x. (Int -> x, Int -> x) -> (Int -> SeqF x)
phi1 (nats, squares) n = NextF n (squares n)
phi2 (nats, squares) n = NextF (n * n) (nats (n + 1))

-- anaM :: (forall x. (a -> R x) -> (a -> R (F x))) -> (a -> R (Nu F))
-- Specialized for the product category with R = Delta
anaM1 :: 
    (forall x. (a -> x, b -> x) -> (a -> SeqF x)) ->
    (forall x. (a -> x, b -> x) -> (b -> SeqF x)) -> 
    (a -> Nu SeqF)
anaM1 phi1 phi2 n = Out $ phi1 (anaM1 phi1 phi2, anaM2 phi1 phi2) n
anaM2 :: 
    (forall x. (a -> x, b -> x) -> (a -> SeqF x)) ->
    (forall x. (a -> x, b -> x) -> (b -> SeqF x)) -> 
    (b -> Nu SeqF)
anaM2 phi1 phi2 n = Out $ phi2 (anaM1 phi1 phi2, anaM2 phi1 phi2) n

test1 = take 10 $ toList $ anaM1 phi1 phi2 3
