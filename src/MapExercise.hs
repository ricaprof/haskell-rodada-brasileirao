module MapExercise where

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:t) = 1 + tamanho t

pertence :: Int -> [Int] -> Bool
pertence _ [] = False
pertence x (h:t) = x == h || pertence x t

depositar :: Int -> Int -> Int
depositar valor saldoAtual = valor + saldoAtual

sacar :: Int -> Int -> Int
sacar valor saldoAtual | valor <= saldoAtual = saldoAtual - valor | otherwise = saldoAtual

aplicar :: Int -> Int -> Int
aplicar valor saldoAtual | valor >= 0 = depositar valor saldoAtual | otherwise = sacar (abs valor) saldoAtual

rendimento :: Int -> Int
rendimento saldoAtual = saldoAtual + saldoAtual `div` 100

criarTarifa :: Int -> Int -> Int
criarTarifa valor saldoAtual = saldoAtual - valor

tarifaBasica :: Int -> Int
tarifaBasica = criarTarifa 5

tarifaPremium :: Int -> Int
tarifaPremium = criarTarifa 30

aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes f x = f (f x)

saldo :: [Int]
saldo = [1000, 2000, 3000, 0]

nossoMap :: (Int -> Int) -> [Int] -> [Int]
nossoMap _ [] = []
nossoMap f (h:t) = f h : nossoMap f t

main :: IO ()
main = do
  print (rendimento 1000)
  print (tarifaBasica (tarifaBasica 1000))
  print (aplicaDuasVezes tarifaPremium 1000)
  print (nossoMap rendimento saldo)
