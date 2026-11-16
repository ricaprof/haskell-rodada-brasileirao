{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Char (isSpace)
import Data.List (intercalate, nub, sortOn)
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
times = foldl adicionar []
  where adicionar ts p = foldl inserir ts [mandante p, visitante p]
        inserir ts t = if t `elem` ts then ts else ts ++ [t]

saldo :: Linha -> Int
saldo l = golsPro l - golsContra l

linhaDoTime :: Time -> [Partida] -> Linha
linhaDoTime t = foldl acumular (Linha t 0 0 0 0 0 0 0)
  where
    acumular l p = case resultadoPara t p of
      Nothing -> l
      Just r -> Linha t (jogos l + 1) (vitorias l + if r == Vitoria then 1 else 0) (empates l + if r == Empate then 1 else 0) (derrotas l + if r == Derrota then 1 else 0) (golsPro l + feitos) (golsContra l + sofridos) (pontosLinha l + pontos r)
      where (feitos, sofridos) = if mandante p == t then (golsMandante p, golsVisitante p) else (golsVisitante p, golsMandante p)

aproveitamento :: Linha -> Double
aproveitamento l | jogos l == 0 = 0 | otherwise = fromIntegral (pontosLinha l) / fromIntegral (3 * jogos l)

classificacao :: [Partida] -> [Linha]
classificacao ps = sortOn criterio (map (`linhaDoTime` ps) (times ps))
  where criterio l = (Down (pontosLinha l), Down (vitorias l), Down (saldo l), time l)

ateRodada :: Int -> [Partida] -> [Partida]
ateRodada r = filter ((<= r) . rodada)

maiorGoleada :: [Partida] -> Maybe Partida
maiorGoleada [] = Nothing
maiorGoleada (p:ps) = Just (foldl escolher p ps)
  where escolher a b = if diferenca b > diferenca a then b else a
        diferenca x = abs (golsMandante x - golsVisitante x)

mediaGols :: [Partida] -> Double
mediaGols [] = 0
mediaGols ps = fromIntegral (sum (map golsDaPartida ps)) / fromIntegral (length ps)
  where golsDaPartida p = golsMandante p + golsVisitante p

data Erro = Erro { numeroLinha :: Int, mensagem :: String } deriving (Eq, Show)

lerPartida :: Int -> String -> Either [Erro] Partida
lerPartida n texto = case map trim (separarPontoEVirgula texto) of
  [r, casa, gc, gf, fora] | null casa || null fora -> Left [Erro n "os nomes dos times não podem ser vazios"]
                          | casa == fora -> Left [Erro n "um time não pode jogar contra si mesmo"]
                          | otherwise -> validar r gc gf casa fora
  campos -> Left [Erro n ("quantidade incorreta de campos: esperado 5, encontrado " ++ show (length campos))]
  where
    validar r gc gf casa fora = do
      r' <- ler "rodada" r
      gc' <- ler "gols do mandante" gc
      gf' <- ler "gols do visitante" gf
      Right (Partida r' casa gc' gf' fora)
      where
        ler campo valor = case readMaybe valor of
          Just x | x >= 0 -> Right x
          _ -> Left [Erro n (campo ++ " inválidos: " ++ show valor ++ " (use um inteiro não negativo)")]

lerCampeonato :: String -> Either [Erro] [Partida]
lerCampeonato conteudo = let rs = map (uncurry lerPartida) (linhasComNumero conteudo); es = concat [e | Left e <- rs]; ps = [p | Right p <- rs]; todos = es ++ errosDeConfrontos ps in if null todos then Right ps else Left (sortOn numeroLinha todos)

errosDeConfrontos :: [Partida] -> [Erro]
errosDeConfrontos ps = concatMap porRodada (nub (map rodada ps))
  where
    porRodada r = map (\t -> Erro 0 ("time participa mais de uma vez na rodada " ++ show r ++ ": " ++ t)) (repetidos (concatMap timesDaPartida (filter ((== r) . rodada) ps)))
    timesDaPartida p = [mandante p, visitante p]
    repetidos xs = nub [x | x <- xs, length (filter (== x) xs) > 1]

todosOsErros :: String -> [Erro]
todosOsErros s = case lerCampeonato s of Right _ -> []; Left es -> es

linhasUteis :: String -> [String]
linhasUteis = map snd . linhasComNumero

linhasComNumero :: String -> [(Int, String)]
linhasComNumero = filter (\(_, l) -> let t = trim l in not (null t) && take 1 t /= "#") . zip [1..] . lines

separarPontoEVirgula :: String -> [String]
separarPontoEVirgula [] = [""]
separarPontoEVirgula (x:xs) | x == ';' = "" : separarPontoEVirgula xs
                            | otherwise = case separarPontoEVirgula xs of
                                h:t -> (x:h):t
                                [] -> [[x]]

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
        eh2019 l = let c = separarCSV l in length c > 2 && "2019" `terminaEm` trim (c !! 2)
        terminaEm sufixo texto = reverse sufixo == take (length sufixo) (reverse texto)

mostrarLinha :: Int -> Linha -> String
mostrarLinha n l = intercalate " | " [show n, time l, show (jogos l), show (vitorias l), show (empates l), show (derrotas l), show (golsPro l), show (golsContra l), show (saldo l), show (pontosLinha l)]

main :: IO ()
main = do
  conteudo <- readFile "data/campeonato-brasileiro-full.csv"
  case lerCampeonato (converterCSVHistorico conteudo) of
    Left es -> putStrLn "Erros encontrados na entrada:" >> mapM_ (putStrLn . formatarErro) es
    Right ps -> do
      let temporada = ps
          parcial = classificacao (ateRodada 19 temporada)
      putStrLn "CLASSIFICAÇÃO — CAMPEONATO BRASILEIRO 2019"
      putStrLn "POS | TIME | J | V | E | D | GP | GC | SG | PTS"
      mapM_ (putStrLn . uncurry mostrarLinha) (zip [1..] (classificacao temporada))
      putStrLn "\nLÍDER APÓS A RODADA 19"
      maybe (putStrLn "não há partidas") (putStrLn . mostrarLinha 1) (primeiro parcial)
      putStrLn ("\nPartidas: " ++ show (length temporada) ++ "; média de gols: " ++ show (mediaGols temporada))
      putStrLn ("Maior goleada: " ++ maybe "nenhuma" show (maiorGoleada temporada))
      putStrLn "\nEXEMPLO DE ERRO DE ENTRADA"
      mapM_ (putStrLn . formatarErro) (todosOsErros "1;Time A;dois;0;Time B")
  where
    primeiro [] = Nothing
    primeiro (x:_) = Just x
    formatarErro e = "linha " ++ show (numeroLinha e) ++ ": " ++ mensagem e
