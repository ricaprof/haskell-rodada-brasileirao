
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

-- Remover aspas e espaços
limpar :: String -> String
limpar = filter (\c -> c /= '"' && not (isSpace c))

-- Extrair ano de uma data no formato dia/mes/ano
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

    -- Filtrar partidas da temporada escolhida
    let dadosAno = filter (\linha ->
            length linha >= 14 &&
            extrairAno (linha !! 2) == ano
            ) dados

    -- Calcular pontos de mandante e visitante
    let novosDados = map (\linha ->
            let gol1 = readMaybe (limpar (linha !! 12)) :: Maybe Int
                gol2 = readMaybe (limpar (linha !! 13)) :: Maybe Int

                (pm, pv) = case (gol1, gol2) of
                    (Just g1, Just g2) ->
                        (show (calcularPontos g1 g2),
                         show (calcularPontos g2 g1))
                    _ -> ("", "")

            in linha ++ [pm, pv]
            ) dadosAno

    -- Transformar cada partida em dois registros
    let registros = concatMap (\linha ->
            let mandante = limpar (linha !! 4)
                visitante = limpar (linha !! 5)

                pm = readMaybe (last (init linha)) :: Maybe Int
                pv = readMaybe (last linha) :: Maybe Int

            in case (pm, pv) of
                (Just p1, Just p2) ->
                    [(mandante, p1), (visitante, p2)]
                _ -> []
            ) novosDados

    -- Agrupar por time e somar os pontos
    let classificacao = Map.fromListWith (+) registros

    -- Ordenar do maior para o menor
    let ranking = sortOn (Down . snd) (Map.toList classificacao)

    putStrLn ("CLASSIFICACAO - BRASILEIRAO " ++ ano)
    putStrLn "--------------------------------"

    mapM_ (\(time, pontos) ->
        putStrLn (time ++ ": " ++ show pontos ++ " pontos")
        ) ranking

    -- Salvar partidas com os pontos calculados
    let resultado =
            (cabecalho ++ ["pontos_mandante", "pontos_visitante"])
            : novosDados

    writeFile ("data/campeonato-" ++ ano ++ "-pontos.csv")
        (unlines (map (intercalate ",") resultado))
