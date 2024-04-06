module CallCC where
    
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

test = flip runContT return $ do
    lift $ putStrLn "before"
    (f1, num) <- callCC $ \k -> let f x = (lift $ putStrLn "evaluating k1")  >> k (f, x)
                                in do 
                                    lift $ putStrLn "binding f1 to f, and num to 0" 
                                    return (f, 0)
    lift $ putStrLn "continuing "
    if num < 2
        then f1 (num + 1) >> (lift $ putStrLn "notprinted ") >> return ()
        else lift $ print num
    if num < 2
        then(lift $ putStrLn "strictly less than 2 ")
        else(lift $ putStrLn "more than 2 ")
main = do
    test
    putStrLn "Done"

k :: (Integer -> ContT () IO ()) -> (Integer -> ContT () IO ())
k f num = do
    lift $ putStrLn "continuing "
    if num < 2
        then f (num + 1) >> return ()
        else lift $ print num

k1 = k k1 


y :: forall a  t. ((a ->  t) ->(a -> t)) -> (a -> t)
y k = k (y k)

f :: Integer -> ContT () IO ()
f  = y k

{-
\k ->   let f x = (lift $ putStrLn "evaluating k1")  >> k (f, x)
        in do 
            lift $ putStrLn "binding k1 to f" 
            return (f, 0)
-}
