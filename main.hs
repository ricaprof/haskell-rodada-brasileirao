import Data.Char (isSpace)
import Data.List (intercalate)
import Text.Read (readMaybe)

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

    -- Calcular pontos do mandante e visitante
    let novosDados = map (\linha ->
            let gol1 = readMaybe (limpar (linha !! 12)) :: Maybe Int
                gol2 = readMaybe (limpar (linha !! 13)) :: Maybe Int

                (pontosMandante, pontosVisitante) =
                    case (gol1, gol2) of
                        (Just g1, Just g2) ->
                            (show (calcularPontos g1 g2),
                             show (calcularPontos g2 g1))
                        _ -> ("", "")

            in linha ++ [pontosMandante, pontosVisitante]
            ) (filter (\linha -> length linha >= 14) dados)

    -- Criar tabela com novas colunas
    let resultado =
            (cabecalho ++ ["pontos_mandante", "pontos_visitante"])
            : novosDados

    -- Mostrar as primeiras 5 partidas
    mapM_ (putStrLn . intercalate ",") (take 6 resultado)

    -- Salvar resultado em outro CSV
    writeFile "data/campeonato-com-pontos.csv"
        (unlines (map (intercalate ",") resultado))