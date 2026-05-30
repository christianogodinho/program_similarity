import System.Environment (getArgs)
import Data.List (nub, sort, sortBy)
import Data.Ord (comparing)

main :: IO ()
main = do
    
    args <- getArgs
    let resFile = args !! 0
    let sepFile = args !! 1
    let c1File = args !! 2
    let c2File = args !! 3

    -- Palavras reservadas
    resConteudo <- readFile resFile
    let reserved = words resConteudo
    
    -- Separadores
    seps <- readFile sepFile
    
    -- Programas
    txt1 <- readFile c1File
    txt2 <- readFile c2File

    let tokens1 = words [if c `elem` seps then ' ' else c | c <- txt1]
    let tokens2 = words [if c `elem` seps then ' ' else c | c <- txt2]

    -- Montando frequência com os respectivos pesos para palavras reservadas e não reservadas
    let palavrasUnicas1 = nub (sort tokens1)
    let palavrasUnicas2 = nub (sort tokens2)
    
    let freq1 = [(w, sum [if x == w then (if w `elem` reserved then 2.0 else 1.0) else 0.0 | x <- tokens1]) | w <- palavrasUnicas1]
    let freq2 = [(w, sum [if x == w then (if w `elem` reserved then 2.0 else 1.0) else 0.0 | x <- tokens2]) | w <- palavrasUnicas2]

    -- Ordenação do relatório: decrescente por frequência e, em caso de empate, alfabética (lexicográfica)
    let freq1Ordenada = sortBy (\(w1, f1) (w2, f2) -> 
                            case compare f2 f1 of
                                EQ -> compare w1 w2
                                res -> res) freq1

    putStrLn "===== Relatório de Frequências de c1 ====="
    mapM_ (\(w, f) -> putStrLn (w ++ " -> " ++ show f)) freq1Ordenada

    -- Cálculo de similaridade com a regra de tolerância de até 10% (|f1 - f2| <= f1 * 0.10)
    let total = sum [f | (_, f) <- freq1]
    
    let matched = sum [f1 | (w, f1) <- freq1, 
                            let f2 = case lookup w freq2 of 
                                        Just v  -> v
                                        Nothing -> 0.0,
                            abs (f1 - f2) <= (f1 * 0.10)]
     
    let similarity = if total == 0 then 0 else matched / total

    putStrLn "\n===== Similaridade Final ====="
    putStrLn ("similarity = " ++ show similarity)
    