module Aula where
import System.Random
import System.IO.Error
{-
getLine :: IO String
getLine = getChar >>= (\x -> if x == '\n'
                             then return []
                             else getLine >>=(\xs -> return (x:xs))
-}

adivinha :: IO ()
adivinha = do putStr "Qual é o número máximo?"
              n <- getLine 
              n1 <- tryIOError ((readIO n) :: IO Int)
              case n1 of
                Left _ -> do putStr "Erro. Vou assumir 10."
                             x <- randomIO (1,10)
                             y <- joga x 0
                             putStrLn ("Usou "++(show y)++" tentativas.")
                Right r -> do putStr "Sem Erro"
                              x <- randomIO (1,r)
                              y <- joga x 0
                              putStrLn ("Usou "++(show y)++" tentativas")

              x <- randomRIO (1, read n)
              y <- joga x 0
              putStrLn ("Usou "++(show y)++" tentativas")

joga :: Int -> Int -> IO Int
joga x n = do putStr "Indique o número: "
              s <- getLine
              r <- readIO s 
              if r == x
              then return (n+1)
              else if (r>x)
                   then putStr "É alto..." >> joga x (n+1)
                   else do putStr "É baixo..." 
                           joga x (n+1)