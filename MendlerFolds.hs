module MendlerFolds where
import Control.Monad
import Control.Monad.State

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

-- Yoneda of the (ListF Int) algebra with [Int] carrier
-- ListF Int [Int] -> [Int]
psi :: forall x . (x -> [Int]) -> ListF Int x -> [Int]
psi k Nil = []
psi k (Cons a x) = 
        if a == 0 
        then []
        else a : (k x)

-- cata :: Functor f => (f a -> a) -> Mu f -> a
-- Yoneda (f a -> a) ~ forall x. (x -> a) -> (f x -> a)
cataM :: (forall x . (x -> a) -> f x -> a) -> Mu f -> a
cataM psi = psi (cataM psi) . in'

-- Monadic version of psi
psiM :: forall x . (x -> IO ()) -> ListF Int x -> IO ()
psiM k Nil = putStrLn "End "
psiM k (Cons a x) = 
        if a == 0 
        then putStrLn "Zero "
        else print a >> k x

test :: [Int]
test = cataM psi (fromList [3, 2 ..])

test1 = cataM psiM (fromList [3, 2 ..])

main = print test

-- another monadic version
mcata :: (Traversable f, Monad m) => (f a -> m a) -> Mu f -> m a
mcata alg = join . fmap alg . sequence . fmap (mcata alg) . in'