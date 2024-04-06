module IterCallCC where
import Control.Monad.CC
import Control.Monad.Cont
import Data.Maybe
import Data.List


for :: Monad m => [a] -> (a -> m b) -> m ()
for lst f = foldLst (\a l -> f a >> l) 
               (return ()) 
               lst

foldLst :: (a -> b -> b) -> b -> [a] -> b
foldLst f z [] = z 
foldLst f z (a:as) = f a (foldLst f z as)

data Iterator m a = Done | Cur a (m (Iterator m a))


begin :: MonadDelimitedCont p s m => [a] -> m (Iterator m a)
begin t = reset $ \p ->
            for t (\a ->
                shift p (\k -> return (Cur a (k $ return ())))) >> return Done

current :: Iterator m a -> Maybe a
current Done      = Nothing
current (Cur a _) = Just a

next :: (a ~ Int, Monad m) => Iterator m a -> m (Iterator m a)
next Done      = return Done
next (Cur a i) = if a == 0 then return Done else i

finished :: Iterator m a -> Bool
finished Done = True
finished _    = False

main :: IO ()
main = runCCT $ do i <- begin [4, 3 ..]
                   doStuff i
 where
 doStuff i
    | finished i = return ()
    | otherwise  = do i'  <- next i
                      liftIO $ print (fromJust $ current i :: Int)
                      doStuff i'

