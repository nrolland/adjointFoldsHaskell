{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Functors where

-- Higher-order functors and adjunctions
-- Perfect trees

import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor.Const (Const (..))

-- Natural transformations
type f :~> g = forall a. f a -> g a
infixr 0 :~>

-- Higher-order functor in the functor category
class HFunctor ff where
    -- The result is a functor
    ffmap :: forall f a b. Functor f => (a -> b) -> ff f a -> ff f b
    -- Lifting natural transformations
    hfmap :: (f :~> g) -> ff f :~> ff g

instance (HFunctor ff, Functor f) => Functor (ff f) where
    fmap = ffmap

instance HFunctor PerfectF where
    ffmap :: Functor f => (a -> b) -> PerfectF f a -> PerfectF f b
    ffmap h (Zero a) = Zero (h a)
    ffmap h (Succ faa) = Succ $ fmap (bimap h h) faa
    hfmap :: (f :~> g) -> PerfectF f :~> PerfectF g
    hfmap _ (Zero x) = Zero x
    hfmap h (Succ faa) = Succ $ h faa

newtype Mu ff a = In { in' :: ff (Mu ff) a }

-- The fixed point of the higher-order functor is a functor.
instance HFunctor ff => Functor (Mu ff) where
    fmap :: HFunctor ff => (a -> b) -> Mu ff a -> Mu ff b
    fmap h (In ffmuffa) = In $ ffmap h ffmuffa

data Perfect a = Zro a | Suc (Perfect (a, a))

data PerfectF f a = Zero a | Succ (f (a, a))

-- Smart constructors
zero :: a -> Mu PerfectF a
zero a = In $ Zero a

suc :: Mu PerfectF (a, a) -> Mu PerfectF a
suc pairs = In (Succ pairs)

tree1 :: Mu PerfectF Int
tree1 = zero 0

tree2 :: Mu PerfectF Int
tree2 = suc (zero (1, 2))

tree4 :: Mu PerfectF Int
tree4 = suc (suc (zero ((1, 2), (3, 4))))

-- Mendler-style higher-order catamorphism.
cataM :: forall f ff. HFunctor ff =>
    (forall g. Functor g => (g :~> f) -> ff g :~> f) -> Mu ff :~> f
cataM psi = psi (cataM psi) . in'

psi :: Functor g => g :~> Const Int -> PerfectF g :~> Const Int
psi _ (Zero _) = Const 1
psi h (Succ gaa) = Const $ 2 * getConst (h gaa)

-- Traverse the tree and return a list of leaves.
psi1 :: Functor g => g :~> [] -> PerfectF g :~> []
psi1 _ (Zero a) = [a]
psi1 h (Succ gaa) = unpair (h gaa)

unpair :: [(a, a)] -> [a]
unpair [] = []
unpair ((x, y) : rest) = x : y : unpair rest

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Functor, Show)

psi2 :: Functor g => g :~> Tree -> PerfectF g :~> Tree
psi2 _ (Zero a) = Leaf a
psi2 h (Succ gaa) = uptree (h gaa)

uptree :: Tree (a, a) -> Tree a
uptree (Leaf (x, y)) = Branch (Leaf x) (Leaf y)
uptree (Branch t1 t2) = Branch (uptree t1) (uptree t2)

test :: Int
test = getConst $ cataM psi tree4

test2 :: [Int]
test2 = cataM psi1 tree4

test3 :: Tree Int
test3 = cataM psi2 tree4

data FList f a = FNil | FCons a (f a)

data FunList a = FunNil | FunConst a (FunList a)

instance HFunctor FList where
    ffmap :: Functor f => (a -> b) -> FList f a -> FList f b
    ffmap _ FNil = FNil
    ffmap h (FCons a fa) = FCons (h a) (fmap h fa)
    hfmap :: (f :~> g) -> FList f :~> FList g
    hfmap _ FNil = FNil
    hfmap nat (FCons a fa) = FCons a (nat fa)

-- [C, C] -> C
class FromFunctor (hf :: (* -> *) -> *) where
    hmap :: (f :~> g) -> hf f -> hf g

-- C -> [C, C]
class ToFunctor (tf :: * -> (* -> *)) where
    ftmap :: (y -> y') -> tf a y -> tf a y'
    tmap :: (a -> b) -> tf a :~> tf b

instance ToFunctor tf => Functor (tf a) where
    fmap = ftmap

class (FromFunctor lf, ToFunctor rf) => FromToAdj lf rf where
    ltor :: Functor f => (lf f -> y) -> f :~> rf y
    rtol :: (f :~> rf y) -> lf f -> y

class (ToFunctor lf, FromFunctor rf) => ToFromAdj lf rf where
    ltor' :: (lf y :~> f) -> y -> rf f
    rtol' :: Functor f => (y -> rf f) -> lf y :~> f

-- Lshift a -| App a -| Rshift a
newtype App a f = App (f a)

instance FromFunctor (App a) where
    hmap nat (App fa) = App (nat fa)

newtype Rshift a b y = Rshift ((y -> a) -> b)

instance ToFunctor (Rshift x) where
    ftmap :: (y -> y') -> Rshift x a y -> Rshift x a y'
    ftmap h (Rshift k) = Rshift (\y'x -> k (y'x . h))
    tmap :: (a -> b) -> Rshift x a :~> Rshift x b
    tmap h (Rshift k) = Rshift (h . k)

instance FromToAdj (App a) (Rshift a) where
    ltor :: Functor f => (App a f -> y) -> f :~> Rshift a y
    ltor h fa = Rshift (\atox -> h (App $ fmap atox fa))
    rtol :: (f :~> Rshift a b) -> App a f -> b
    rtol nat (App fx) =
      let Rshift k = nat fx
      in k id

data Lshift a b y = Lshift (a -> y) b

instance ToFunctor (Lshift a) where
    ftmap :: (y -> y') -> Lshift a b y -> Lshift a b y'
    ftmap h (Lshift f b) = Lshift (h . f) b
    tmap :: (b -> b') -> Lshift a b :~> Lshift a b'
    tmap h (Lshift f b) = Lshift f (h b)

instance ToFromAdj (Lshift a) (App a) where
    ltor' :: (Lshift a y :~> f) -> y -> App a f
    ltor' nat y = App $ nat (Lshift id y)
    rtol' :: Functor f => (y -> App a f) -> Lshift a y :~> f
    rtol' h (Lshift f b) =
      let App fa = h b
      in fmap f fa

hcata :: HFunctor hf => (hf f :~> f) -> Mu hf :~> f
hcata alg = alg . hfmap (hcata alg) . in'

-- Algebra transformer through the adjunction.
cataT :: (FromToAdj lf rf, HFunctor f) =>
    (lf (f (rf a)) -> a) -> lf (Mu f) -> a
cataT alg = rtol (hcata (ltor alg))

alg :: App Int (FList (Rshift Int Int)) -> Int
alg (App FNil) = 0
alg (App (FCons n (Rshift k))) = k (+ n)

lst :: Mu FList Int
lst = In (FCons 100 (In (FCons 10 (In (FCons 1 (In FNil))))))

aplst :: App Int (Mu FList)
aplst = App lst

testS :: Int
testS = cataT alg aplst

-- Summing a perfect tree with a direct recursive formula.
sump :: Mu PerfectF Int -> Int
sump (In (Zero n)) = n
sump (In (Succ p)) = sump (fmap (uncurry (+)) p)

sumAlg :: App Int (PerfectF (Rshift Int Int)) -> Int
sumAlg (App (Zero n)) = n
sumAlg (App (Succ (Rshift k))) = k (uncurry (+))

testsp :: Int
testsp = cataT sumAlg (App tree4)
