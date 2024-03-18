 -- Right 3 >>= (\x -> return x*2) 
 -- Right 3 >>= (return . (2*))

 -- getLine >>= (putStrLn . tail)