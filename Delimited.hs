module Delimited where
import Control.Monad.CC
import Control.Monad.Cont
import Data.Maybe

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

empty = Leaf
singleton a = Branch a Leaf Leaf

insert :: Ord t => t -> Tree t -> Tree t
insert b Leaf = Branch b Leaf Leaf
insert b (Branch a l r)
    | b < a = Branch a (insert b l) r
    | otherwise = Branch a l (insert b r)

fold :: (a -> b -> b -> b) -> b -> Tree a -> b
fold f z Leaf = z
fold f z (Branch a l r) = f a (fold f z l) (fold f z r)

for :: Monad m => Tree a -> (a -> m b) -> m ()
for t f = fold (\a l r -> l >> f a >> r) 
               (return ()) 
               t


data Iterator m a = Done | Cur a (m (Iterator m a))

{-
k x = for t (\a -> x) >> return Done
g k = return (Cur a (k $ return ()))

k $ return () = for t (\a -> return ()) >> return Done

g k = return (Cur a (for t (\a -> return ()) >> return Done))
-}
begin :: MonadDelimitedCont p s m => Tree a -> m (Iterator m a)
begin t = reset $ \p ->
            for t (\a ->
                shift p (\k -> return (Cur a (k $ return ())))) >> return Done

current :: Iterator m a -> Maybe a
current Done      = Nothing
current (Cur a _) = Just a

next :: Monad m => Iterator m a -> m (Iterator m a)
next Done      = return Done
next (Cur _ i) = i

finished :: Iterator m a -> Bool
finished Done = True
finished _    = False

main :: IO ()
main = runCCT $ do t <- return $ unfoldTree [1, 5, -1, 9]
                   i <- begin t
                   doStuff i
 where
 doStuff i
    | finished i = return ()
    | otherwise  = do i'  <- next i
                      -- i'' <- next i
                      liftIO $ print (fromJust $ current i :: Int)
                      doStuff i'

unfoldTree :: [Int] -> Tree Int
unfoldTree = foldr insert Leaf 
