import Data.Char (isSpace)

separarLinhas :: String -> [String]
separarLinhas [] = [""]
separarLinhas (',' : xs) = "" : separarLinhas xs
separarLinhas (x : xs) =
    let (primeiro : resto) = separarLinhas xs
    in (x : primeiro) : resto

calcularPontos :: Int -> Int -> Int
calcularPontos gol1 gol2
    | gol1 > gol2  = 3
    | gol1 == gol2 = 1
    | otherwise    = 0


limpar :: String -> String
limpar = filter (\c -> c /= '"' && not (isSpace c))

main :: IO ()
main = do
    conteudo <- readFile "data/campeonato-brasileiro-full.csv"

    let linhas = lines conteudo
    let tabela = map separarLinhas linhas

    let cabecalho = head tabela
    let dados = tail tabela

    let novosDados = map (\linha ->
            let gol1 = read (limpar (linha !! 13)) :: Int
                gol2 = read (limpar (linha !! 14)) :: Int
                pontos = calcularPontos gol1 gol2
            in linha ++ [show pontos]
            ) dados

    let pontos = map (\linha -> read (last linha) :: Int) novosDados

    let acumulados = scanl1 (+) pontos

    let tabelaFinal = zipWith
            (\linha total -> linha ++ [show total])
            novosDados
            acumulados

    let resultado = (cabecalho ++ ["pontos", "acumulados"]) : tabelaFinal

    mapM_ print resultado
    print ("dajo")
    print ( resultado !! 1 )