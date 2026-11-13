separarLinhas :: String -> [String]
separarLinhas [] = [""]
separarLinhas (',' : xs) = "" : separarLinhas xs
separarLinhas (x : xs) =
    let (primeiro : resto) = separarLinhas xs
    in (x : primeiro) : resto

main :: IO ()
main = do
    conteudo <- readFile "data/campeonato-brasileiro-full.csv"
    let linhas = lines conteudo
    let tabela = map separarLinhas linhas
    print tabela