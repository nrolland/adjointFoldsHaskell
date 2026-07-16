# Adjoint folds in Haskell

This repository is a small laboratory for **adjoint folds** and related
higher-order recursion schemes in Haskell.

Adjoint folds generalise familiar folds by using an adjunction to move between
the domain in which a recursive datatype lives and the domain in which its
algebra is easiest to express. The construction covers, or helps explain,
several recursion patterns usually introduced separately, including
catamorphisms, histomorphisms and mutumorphisms.

The main reference is Ralf Hinze's
[Adjoint Folds and Unfolds](https://www.cs.ox.ac.uk/ralf.hinze/SSGIP10/AdjointFolds.pdf).

> Status: experimental. The code currently favours explicit encodings and
> type-level exploration over a polished public API.

## Running the examples

The repository deliberately keeps the build lightweight. With GHC installed:

```bash
./test.sh
```

The script loads the source modules in GHCi and checks the current regression
examples. The same command runs in GitHub Actions.

## Source modules

- `Functors.hs` contains natural transformations, higher-order functors,
  fixed points, adjunctions, perfect-tree examples and adjoint folds.
- `MutuAna.hs` contains a small mutual-anamorphism example.

Each module declares the language extensions it needs, so loading the source
does not depend on hidden GHCi settings.

## The central idea

For an ordinary fixed point `Mu f`, a catamorphism consumes an algebra

```haskell
f a -> a
```

and recursively replaces every constructor layer with that algebra.

For a higher-order functor `hf`, the analogous fold is:

```haskell
hcata :: HFunctor hf => (hf f :~> f) -> Mu hf :~> f
```

where

```haskell
type f :~> g = forall a. f a -> g a
```

is a natural transformation.

An adjoint fold starts from an adjunction `L ⊣ R`. Instead of requiring an
ordinary algebra directly, it accepts an algebra of shape

```haskell
L (F (R a)) -> a
```

The adjunction transports that algebra into

```haskell
F (R a) :~> R a
```

which can be consumed by `hcata`. The result is then transported back:

```haskell
cataT
  :: (FromToAdj l r, HFunctor f)
  => (l (f (r a)) -> a)
  -> l (Mu f)
  -> a

cataT alg = rtol (hcata (ltor alg))
```

Read the implementation from right to left:

1. `ltor alg` turns the transformed algebra into an ordinary higher-order
   algebra.
2. `hcata` folds the fixed point into `r a`.
3. `rtol` moves the result back across the adjunction.

This factorisation is the useful part of the construction: recursion remains
concentrated in `hcata`, while the adjunction describes the extra context
needed by a more expressive fold.

## Core encodings

### Natural transformations

```haskell
type f :~> g = forall a. f a -> g a
```

A value of this type must work uniformly for every `a`.

### Higher-order functors

```haskell
class HFunctor ff where
  ffmap :: Functor f => (a -> b) -> ff f a -> ff f b
  hfmap :: (f :~> g) -> ff f :~> ff g
```

`ffmap` maps values inside the resulting functor. `hfmap` maps the recursive
functor argument itself.

### Higher-order fixed points

```haskell
newtype Mu ff a = In { in' :: ff (Mu ff) a }
```

`Mu ff` ties the recursive knot of a higher-order functor.

## Worked example: perfect trees

The repository represents perfect trees with:

```haskell
data PerfectF f a
  = Zero a
  | Succ (f (a, a))
```

Every `Succ` level pairs the elements below it, so all leaves occur at the same
depth. Example smart constructors build trees containing one, two and four
leaves.

Several algebras demonstrate different interpretations of the same tree:

- count its leaves;
- flatten its leaves into a list;
- convert it to an ordinary binary tree;
- sum its leaves using an adjoint fold.

The direct recursive sum is:

```haskell
sump :: Mu PerfectF Int -> Int
sump (In (Zero n)) = n
sump (In (Succ p)) = sump (fmap (uncurry (+)) p)
```

The adjoint version packages evaluation at `Int` in `App Int` and uses its
right adjoint `Rshift Int`:

```haskell
sumAlg :: App Int (PerfectF (Rshift Int Int)) -> Int
sumAlg (App (Zero n)) = n
sumAlg (App (Succ (Rshift k))) = k (uncurry (+))
```

The regression suite checks:

```haskell
cataT sumAlg (App tree4) == sump tree4
```

## Mendler-style folds

The repository also experiments with a Mendler-style higher-order fold:

```haskell
cataM
  :: HFunctor ff
  => (forall g. Functor g => (g :~> f) -> ff g :~> f)
  -> Mu ff :~> f
```

The recursive call is supplied abstractly to the algebra instead of exposing
the recursive datatype. This restriction can make termination and proof
arguments easier, and provides a useful comparison with the adjunction-based
encoding.

## Current limitations

The implementation is exploratory and currently has several deliberate rough
edges:

- most higher-order definitions still live in one source module;
- the generic `Functor (ff f)` instance requires unusually permissive instance
  resolution;
- the tests cover representative examples, not algebraic laws;
- names such as `psi`, `psi1` and `psi2` remain concise rather than
  tutorial-friendly.

A sensible next refactoring is to split natural transformations,
higher-order functors, adjunctions, fixed points and examples into independent
modules, while keeping the regression suite green after each move.

## Suggested future layout

```text
src/
  AdjointFolds/
    NaturalTransformation.hs
    HigherFunctor.hs
    Fixpoint.hs
    Adjunction.hs
    PerfectTree.hs
    Examples.hs
test/
  Main.hs
```

## Bibliography

- Ralf Hinze,
  [Adjoint Folds and Unfolds: Scything Through the Thicket of Morphisms](https://www.cs.ox.ac.uk/ralf.hinze/SSGIP10/AdjointFolds.pdf)
- Nicolas Wu,
  [Conjugate Hylomorphisms: The Mother of All Structured Recursion Schemes](https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Hylomorphisms.pdf)
- Patrick Bahr and Tom Hvitved,
  [Unifying Structured Recursion Schemes](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/urs.pdf)
- [Fantastic Morphisms and Where to Find Them: A Guide to Recursion Schemes](https://arxiv.org/abs/2202.13633)
