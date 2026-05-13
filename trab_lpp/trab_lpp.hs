import System.Environment (getArgs)
import Data.Char (toLower)
import Data.List (nub, sort)

main :: IO ()
main = do
    -- Pega os argumentos 
    args <- getArgs
    let resFile = args !! 0
    let sepFile = args !! 1
    let c1File = args !! 2
    let c2File = args !! 3

    -- Lê tudo de uma vez
    resConteudo <- readFile resFile
    let reserved = words (map toLower resConteudo)
    
    -- Lê separadores
    seps <- readFile sepFile
    
    -- Lê programas para análise
    txt1 <- readFile c1File
    txt2 <- readFile c2File

    -- Atenção com redução para lowerCase. Algumas liguagens possuem diferenciações entre palavras reservadas que começam em maiúsculo
    -- e variáveis que podem ter o nome de palavras reservadas mas não são por estarem em minúsculo
    -- A comparação deve ser válida para toda e qualquer linguagem de programação enviada para análise
    -- Removendo separadores dos códigos a serem analisados 
    let tokens1 = words [if c `elem` seps then ' ' else toLower c | c <- txt1]
    let tokens2 = words [if c `elem` seps then ' ' else toLower c | c <- txt2]

    -- Montando frequencia com os respectivos pesos para palavras reservadas e não reservadas
    -- Otimizar leitura e análise dos arquivos com a implementação de uma função melhor otimizada para fazer a leitura e comparação
    -- de forma mais 'simultânea'
    let palavrasUnicas = nub (sort tokens1) -- Ordena e depois remove repetidos
    let freq1 = [(w, sum [if x == w then (if w `elem` reserved then 2.0 else 1.0) else 0.0 | x <- tokens1]) | w <- palavrasUnicas]
    let freq2 = [(w, sum [if x == w then (if w `elem` reserved then 2.0 else 1.0) else 0.0 | x <- tokens2]) | w <- nub (sort tokens2)]

    -- Print do relatório 
    -- Pensar numa melhor exibição dos resultados coletados, levando em consideração a ordenação exigida no trabalho
    putStrLn "===== Frequencias de c1 (versao embrionaria) ====="
    -- Função lambda para execução do print em cada tupla de freq1
    mapM_ (\(w, f) -> putStrLn (w ++ " -> " ++ show f)) freq1

    -- Cálculo de similaridade simplão
    -- Implementar o cálculo correto com a consideração dos 10%
    -- Exemplo: c1 tem 1 ocorrência de 'ant' e c2 tem 100 ocorrências de 'ant', ou seja, similaridade praticamente irrelevante
    let total = sum [f | (_, f) <- freq1]
    let matched = sum [min f1 (case lookup w freq2 of 
                                Just f2 -> f2; 
                                Nothing -> 0) 
                                | (w, f1) <- freq1]
    
    let similarity = if total == 0 then 0 else matched / total

    putStrLn "\n===== Similaridade (prova de conceito) ====="
    putStrLn ("similarity = " ++ show similarity)