
import Data.Char (isSpace)
import Data.List (intercalate, sortOn)
import Data.Ord (Down(..))
import Text.Read (readMaybe)
import qualified Data.Map.Strict as Map

-- Separar colunas do CSV
separarLinhas :: String -> [String]
separarLinhas [] = [""]
separarLinhas (',' : xs) = "" : separarLinhas xs
separarLinhas (x : xs) =
    let (primeiro : resto) = separarLinhas xs
    in (x : primeiro) : resto

-- Calcular pontos de uma partida
calcularPontos :: Int -> Int -> Int
calcularPontos gol1 gol2
    | gol1 > gol2  = 3
    | gol1 == gol2 = 1
    | otherwise    = 0

-- Calcular número de vitórias
calcularVitorias :: Int -> Int -> Int
calcularVitorias gol1 gol2
    | gol1 > gol2 = 1
    | otherwise   = 0

-- Calcular saldo de gols
calcularSaldo :: Int -> Int -> Int
calcularSaldo golsPro golsContra = golsPro - golsContra

-- Remover aspas e espaços
limpar :: String -> String
limpar = filter (\c -> c /= '"' && not (isSpace c))

-- Extrair ano da data
extrairAno :: String -> String
extrairAno dataTexto =
    case reverse (splitData (limpar dataTexto)) of
        (ano:_) -> ano
        _       -> ""
  where
    splitData [] = [""]
    splitData ('/':xs) = "" : splitData xs
    splitData (x:xs) =
        let (primeiro:resto) = splitData xs
        in (x:primeiro) : resto

main :: IO ()
main = do
    conteudo <- readFile "data/campeonato-brasileiro-full.csv"

    let ano = "2019"

    let tabela = map separarLinhas (lines conteudo)
    let cabecalho = head tabela
    let dados = tail tabela

    -- Filtrar temporada
    let dadosAno = filter (\linha ->
            length linha >= 14 &&
            extrairAno (linha !! 2) == ano
            ) dados

    -- Calcular pontos, vitórias e saldo de cada partida
    let novosDados = map (\linha ->
            let gol1 = readMaybe (limpar (linha !! 12)) :: Maybe Int
                gol2 = readMaybe (limpar (linha !! 13)) :: Maybe Int

                (pm, pv, vm, vv, sm, sv) =
                    case (gol1, gol2) of
                        (Just g1, Just g2) ->
                            ( show (calcularPontos g1 g2)
                            , show (calcularPontos g2 g1)
                            , show (calcularVitorias g1 g2)
                            , show (calcularVitorias g2 g1)
                            , show (calcularSaldo g1 g2)
                            , show (calcularSaldo g2 g1)
                            )
                        _ -> ("", "", "", "", "", "")

            in linha ++ [pm, pv, vm, vv, sm, sv]
            ) dadosAno

    -- Cada partida gera dois registros:
    -- (time, (pontos, vitórias, saldo))
    let registros = concatMap (\linha ->
            let mandante = limpar (linha !! 4)
                visitante = limpar (linha !! 5)

                gol1 = readMaybe (limpar (linha !! 12)) :: Maybe Int
                gol2 = readMaybe (limpar (linha !! 13)) :: Maybe Int

            in case (gol1, gol2) of
                (Just g1, Just g2) ->
                    [ (mandante,
                        ( calcularPontos g1 g2
                        , calcularVitorias g1 g2
                        , calcularSaldo g1 g2
                        ))
                    , (visitante,
                        ( calcularPontos g2 g1
                        , calcularVitorias g2 g1
                        , calcularSaldo g2 g1
                        ))
                    ]
                _ -> []
            ) dadosAno

    -- Agrupar por equipe e somar pontos, vitórias e saldo
    let classificacao = Map.fromListWith
            (\(p1, v1, s1) (p2, v2, s2) ->
                (p1 + p2, v1 + v2, s1 + s2))
            registros

    -- Ordenar por pontos, vitórias e saldo de gols
    let ranking = sortOn
            (\(_, (pontos, vitorias, saldo)) ->
                (Down pontos, Down vitorias, Down saldo))
            (Map.toList classificacao)

    putStrLn ("CLASSIFICACAO - BRASILEIRAO " ++ ano)
    putStrLn "------------------------------------------------"
    putStrLn "TIME | PONTOS | VITORIAS | SALDO"
    putStrLn "------------------------------------------------"

    mapM_ (\(time, (pontos, vitorias, saldo)) ->
        putStrLn (
            time ++ " | " ++
            show pontos ++ " | " ++
            show vitorias ++ " | " ++
            show saldo
        )
        ) ranking

    -- Criar CSV com novas colunas
    let resultado = (cabecalho ++
            [ "pontos_mandante"
            , "pontos_visitante"
            , "vitorias_mandante"
            , "vitorias_visitante"
            , "saldo_mandante"
            , "saldo_visitante"
            ]) : novosDados

    -- Salvar CSV
    writeFile ("data/campeonato-" ++ ano ++ "-pontos.csv")
        (unlines (map (intercalate ",") resultado))
