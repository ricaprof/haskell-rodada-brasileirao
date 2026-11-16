{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Char (isSpace)
import Data.List (intercalate, sort, sortOn)
import Data.Ord (Down (..))
import Text.Read (readMaybe)

type Time = String
data Resultado = Vitoria | Empate | Derrota deriving (Eq, Ord, Show, Read)
data Partida = Partida { rodada :: Int, mandante :: Time, golsMandante :: Int, golsVisitante :: Int, visitante :: Time } deriving (Eq, Show, Read)
data Linha = Linha { time :: Time, jogos :: Int, vitorias :: Int, empates :: Int, derrotas :: Int, golsPro :: Int, golsContra :: Int, pontosLinha :: Int } deriving (Eq, Show, Read)

participou :: Time -> Partida -> Bool
participou t p = t == mandante p || t == visitante p

resultadoPara :: Time -> Partida -> Maybe Resultado
resultadoPara t p = uncurry resultado <$> golsDoTime t p
  where
    golsDoTime timeJogador partida
      | timeJogador == mandante partida = Just (golsMandante partida, golsVisitante partida)
      | timeJogador == visitante partida = Just (golsVisitante partida, golsMandante partida)
      | otherwise = Nothing

resultado :: Int -> Int -> Resultado
resultado a b | a > b = Vitoria | a == b = Empate | otherwise = Derrota
pontos :: Resultado -> Int
pontos Vitoria = 3
pontos Empate = 1
pontos Derrota = 0

times :: [Partida] -> [Time]
times = sort . foldl adicionar []
  where adicionar ts p = foldl inserir ts [mandante p, visitante p]
        inserir ts t = if t `elem` ts then ts else ts ++ [t]

saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

linhaDoTime :: [Partida] -> Time -> Linha
linhaDoTime partidas t = foldl acumular (Linha t 0 0 0 0 0 0 0) partidas
  where
    acumular l p = case resultadoPara t p of
      Nothing -> l
      Just r -> Linha t (jogos l + 1) (vitorias l + if r == Vitoria then 1 else 0) (empates l + if r == Empate then 1 else 0) (derrotas l + if r == Derrota then 1 else 0) (golsPro l + feitos) (golsContra l + sofridos) (pontosLinha l + pontos r)
      where (feitos, sofridos) = if mandante p == t then (golsMandante p, golsVisitante p) else (golsVisitante p, golsMandante p)

aproveitamento :: Linha -> Double
aproveitamento l | jogos l == 0 = 0 | otherwise = 100 * fromIntegral (pontosLinha l) / fromIntegral (3 * jogos l)

classificacao :: [Partida] -> [Linha]
classificacao ps = sortOn criterio (map (linhaDoTime ps) (times ps))
  where criterio l = (Down (pontosLinha l), Down (vitorias l), Down (saldo l), time l)

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada r = filter ((<= r) . rodada)

maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada [] = Nothing
maiorGoleada (p:ps) = Just (foldl escolher p ps)
  where escolher a b = if diferenca b > diferenca a then b else a
        diferenca x = abs (golsMandante x - golsVisitante x)

mediaGols :: [Partida] -> Maybe Double
mediaGols [] = Nothing
mediaGols ps = Just (fromIntegral (sum (map golsDaPartida ps)) / fromIntegral (length ps))
  where golsDaPartida p = golsMandante p + golsVisitante p

data Erro
  = CamposInvalidos Int Int
  | NumeroInvalido Int String String
  | PlacarNegativo Int String Int
  | MesmoTime Int Time
  | TimeRepetidoNaRodada Int Int Time
  deriving (Eq, Ord)

instance Show Erro where
  show (CamposInvalidos linha quantidade) = linhaTexto linha ++ "quantidade incorreta de campos: esperado 5, encontrado " ++ show quantidade
  show (NumeroInvalido linha campo valor) = linhaTexto linha ++ campo ++ " inválidos: " ++ show valor ++ " (use um inteiro não negativo)"
  show (PlacarNegativo linha campo valor) = linhaTexto linha ++ campo ++ " não pode ser negativo: " ++ show valor
  show (MesmoTime linha t) = linhaTexto linha ++ "um time não pode jogar contra si mesmo: " ++ t
  show (TimeRepetidoNaRodada linha rodadaAtual t) = linhaTexto linha ++ "time participa mais de uma vez na rodada " ++ show rodadaAtual ++ ": " ++ t

linhaTexto :: Int -> String
linhaTexto linha = "linha " ++ show linha ++ ": "

lerPartida :: Int -> String -> Either Erro Partida
lerPartida n texto = case map trim (separarPontoEVirgula texto) of
  [r, casa, gc, gf, fora] | null casa || null fora -> Left (NumeroInvalido n "nome do time" "vazio")
                          | casa == fora -> Left (MesmoTime n casa)
                          | otherwise -> validar r gc gf casa fora
  campos -> Left (CamposInvalidos n (length campos))
  where
    validar r gc gf casa fora = do
      r' <- ler "rodada" r
      gc' <- ler "gols do mandante" gc
      gf' <- ler "gols do visitante" gf
      Right (Partida r' casa gc' gf' fora)
      where
        ler campo valor = case readMaybe valor of
          Just x | x >= 0 -> Right x
                | otherwise -> Left (PlacarNegativo n campo x)
          _ -> Left (NumeroInvalido n campo valor)

lerCampeonato :: String -> Either Erro [Partida]
lerCampeonato conteudo = do
  partidas <- traverse (uncurry lerPartida) (linhasComNumero conteudo)
  validarRepeticoes (linhasComNumero conteudo) partidas

validarRepeticoes :: [(Int, String)] -> [Partida] -> Either Erro [Partida]
validarRepeticoes numeradas partidas = go [] (zip (map fst numeradas) partidas)
  where
    go _ [] = Right partidas
    go vistos ((linha, p):resto)
      | any (\(r, t) -> r == rodada p && (t == mandante p || t == visitante p)) vistos =
          Left (TimeRepetidoNaRodada linha (rodada p) (timeRepetido vistos p))
      | otherwise = go (vistos ++ [(rodada p, mandante p), (rodada p, visitante p)]) resto
    timeRepetido vistos p = case filter (\(r, t) -> r == rodada p && (t == mandante p || t == visitante p)) vistos of
      ((_, t):_) -> t
      [] -> mandante p

todosOsErros :: String -> [Erro]
todosOsErros conteudo = errosDeLeitura ++ errosDeRepeticao
  where
    numeradas = linhasComNumero conteudo
    resultados = map (uncurry lerPartida) numeradas
    errosDeLeitura = [e | Left e <- resultados]
    validas = [(linha, p) | (linha, Right p) <- zip (map fst numeradas) resultados]
    errosDeRepeticao = repetir [] validas
    repetir _ [] = []
    repetir vistos ((linha, p):resto)
      | any (\(r, t) -> r == rodada p && (t == mandante p || t == visitante p)) vistos =
          TimeRepetidoNaRodada linha (rodada p) (timeRepetido vistos p) : repetir vistos resto
      | otherwise = repetir (vistos ++ [(rodada p, mandante p), (rodada p, visitante p)]) resto
    timeRepetido vistos p = case filter (\(r, t) -> r == rodada p && (t == mandante p || t == visitante p)) vistos of
      ((_, t):_) -> t
      [] -> mandante p

linhasUteis :: String -> [String]
linhasUteis = map snd . linhasComNumero

linhasComNumero :: String -> [(Int, String)]
linhasComNumero = filter (\(_, l) -> let t = trim l in not (null t) && take 1 t /= "#") . zip [1..] . lines

separar :: Char -> String -> [String]
separar _ [] = [""]
separar separador (x:xs) | x == separador = "" : separar separador xs
                          | otherwise = case separar separador xs of
                                h:t -> (x:h):t
                                [] -> [[x]]

separarPontoEVirgula :: String -> [String]
separarPontoEVirgula = separar ';'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace . aspas
  where aspas s = case s of '"':r | not (null r) && last r == '"' -> init r; _ -> s
        dropWhileEnd f = reverse . dropWhile f . reverse

separarCSV :: String -> [String]
separarCSV = go False ""
  where go _ a [] = [a]
        go q a (c:cs) | c == '"' = go (not q) a cs
                       | c == ',' && not q = a : go q "" cs
                       | otherwise = go q (a ++ [c]) cs

converterCSVHistorico :: String -> String
converterCSVHistorico s = unlines (map converter (filter eh2019 (drop 1 (linhasUteis s))))
  where converter l = let c = map trim (separarCSV l) in intercalate ";" [c !! 1, c !! 4, c !! 12, c !! 13, c !! 5]
        eh2019 l = let c = separarCSV l in length c >= 14 && "2019" `terminaEm` trim (c !! 2)
        terminaEm sufixo texto = reverse sufixo == take (length sufixo) (reverse texto)

mostrarLinha :: Int -> Linha -> String
mostrarLinha n l = intercalate " | " [show n, time l, show (jogos l), show (vitorias l), show (empates l), show (derrotas l), show (golsPro l), show (golsContra l), show (saldo l), show (pontosLinha l)]

tabelaFormatada :: [Linha] -> String
tabelaFormatada linhas = unlines (cabecalho : zipWith mostrarLinha [1..] linhas)
  where cabecalho = "POS | TIME | J | V | E | D | GP | GC | SG | PTS"

main :: IO ()
main = do
  conteudo <- readFile "data/campeonato-brasileiro-full.csv"
  case lerCampeonato (converterCSVHistorico conteudo) of
    Left e -> putStrLn "Erro encontrado na entrada:" >> putStrLn (show e)
    Right ps -> do
      let temporada = ps
          parcial = classificacao (ateRodada 19 temporada)
      putStrLn "CLASSIFICAÇÃO — CAMPEONATO BRASILEIRO 2019"
      putStr (tabelaFormatada (classificacao temporada))
      putStrLn "\nLÍDER APÓS A RODADA 19"
      putStr (tabelaFormatada parcial)
      putStrLn ("Partidas: " ++ show (length temporada) ++ "; média de gols: " ++ maybe "indisponível" show (mediaGols temporada))
      putStrLn ("Maior goleada: " ++ maybe "nenhuma" show (maiorGoleada temporada))
      putStrLn "\nEXEMPLO DE ERRO DE ENTRADA"
      mapM_ (putStrLn . show) (todosOsErros "1;Time A;dois;0;Time B")
