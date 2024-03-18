import System.Random
import Data.Char

main = do
    palavas <- readFile "palavras.txt" >>= (return . filter ((>=5) . length) . lines) -- vai estra no discord depois , a função lines transforma numa lista de strings, o filter faz com que só se usem as palavras da lista com 5 ou mais letras
    n <- randomRIO (1, length palavras)
    let palavra = palavras !! n -- usa-se o let quando se quer escrever alguma coisa que não seja uma ação
--incompleto
ciclo :: String -> [Char] -> IO ()
ciclo palavra letras = do
    putStrLn $ unlines [
        esconderPalavra palavra,
        "",
        "1) Adivinhar letra",
        "2) Adivinhar palavra",
        ""
        ]
    opcao <- getLine 
    case opcao of -- quando se entra dentro de um case of tem que se voltar a fazer um do
        "1" -> do 
            putStrLn "Escreve uma letra: "
            letra <- getLine
            if length letra == 1 && isAlpha (head letra) then ciclo palavra (head letra : letras)-- a função isAlpha verifica se o char é uma letra
        else do 
            putStrLn "Letra inválida" -- incompleto
        "2" -> do
             putStrLn "Introduz uma palavra:"
             palavra1 <- getLine
             if palavra1 == palavra then putStrLn "Parabéns, ganhaste!!" else putStrLn ("Errado, perdeste! A palavra era " ++ palavra)

esconderPalavra :: String -> [Char] -> String
esconderPalavra palavra letras = map (\letra -> if letra `elem` then letra else '_') palavra