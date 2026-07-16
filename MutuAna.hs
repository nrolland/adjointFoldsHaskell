{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module MutuAna
  ( Nu (..)
  , SeqF (..)
  , toList
  , nats
  , squares
  , phi1
  , phi2
  , anaM1
  , anaM2
  , test
  , test1
  ) where

newtype Nu f = Out { out' :: f (Nu f) }

data SeqF x = NextF Int x
  deriving (Functor)

toList :: Nu SeqF -> [Int]
toList (Out (NextF n x)) = n : toList x

nats :: Int -> Nu SeqF
nats n = Out (NextF n (squares n))

squares :: Int -> Nu SeqF
squares n = Out (NextF (n * n) (nats (n + 1)))

test :: [Int]
test = take 10 $ toList $ nats 3

-- R = Delta
phi1, phi2 :: forall x. (Int -> x, Int -> x) -> Int -> SeqF x
phi1 (nextNats, nextSquares) n = NextF n (nextSquares n)
phi2 (nextNats, nextSquares) n = NextF (n * n) (nextNats (n + 1))

-- Specialized mutual anamorphism for the product category with R = Delta.
anaM1 ::
    (forall x. (a -> x, b -> x) -> a -> SeqF x) ->
    (forall x. (a -> x, b -> x) -> b -> SeqF x) ->
    a -> Nu SeqF
anaM1 buildA buildB seed =
  Out $ buildA (anaM1 buildA buildB, anaM2 buildA buildB) seed

anaM2 ::
    (forall x. (a -> x, b -> x) -> a -> SeqF x) ->
    (forall x. (a -> x, b -> x) -> b -> SeqF x) ->
    b -> Nu SeqF
anaM2 buildA buildB seed =
  Out $ buildB (anaM1 buildA buildB, anaM2 buildA buildB) seed

test1 :: [Int]
test1 = take 10 $ toList $ anaM1 phi1 phi2 3
