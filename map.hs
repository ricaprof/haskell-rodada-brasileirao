
-- Função para calcular o tamanho de uma lista
tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:t) = 1 + tamanho t

-- Verificar se um valor existe dentro de uma lista
pertence :: Int -> [Int] -> Bool
pertence _  [] = False
pertence x (h:t) 
                | x == h = True
                | otherwise = pertence x t 
-- Depositar um valor na conta bancária
depositar :: Int -> Int -> Int
depositar valor saldo = valor + saldo


--Sacar o valor da conta bancária
sacar :: Int -> Int -> Int
sacar valor saldo
                | valor <= saldo = saldo - valor
                | otherwise = saldo
--Aplicar investimentos
aplicar :: Int -> Int -> Int
aplicar valor saldo
                    | valor >= 0 = depositar valor saldo
                    | otherwise = sacar (abs valor) saldo

--rendimento fixo de 1%
rendimento :: Int -> Int
rendimento saldo = saldo + saldo `div` 100

--Função de tarifa para vários valores
criarTarifa :: Int -> (Int -> Int)
criarTarifa valor = \saldo -> saldo - valor


--tarida de 5 centavos
tarifaBasica :: Int -> Int
tarifaBasica = criarTarifa 5

--tarifa de 30 centavos
tarifaPremium :: Int -> Int
tarifaPremium = criarTarifa 30

--aplica a função duas vezes
aplicaDuasVezes :: (Int -> Int) -> Int -> Int
aplicaDuasVezes funcao saldo = funcao ( funcao saldo)
saldo :: [Int]
saldo = [1000, 2000, 3000, 0]

nossoMap :: (Int -> Int) -> [Int] -> [Int]
nossoMap _ [] = []
nossoMap f (h:t)= f : h nossoMap f t

main :: IO ()
main = do
    print ( rendimento 1000 )
    print ( tarifaBasica (tarifaBasica 1000 ) )
    print ( aplicaDuasVezes tarifaPremium 1000)
    print ( nossoMap rendimento saldo )