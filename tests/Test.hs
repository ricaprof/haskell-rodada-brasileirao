module Test where

import qualified Main as C

assert :: String -> Bool -> IO ()
assert nome cond = if cond then putStrLn ("OK: " ++ nome) else error ("FALHOU: " ++ nome)

partidas :: [C.Partida]
partidas =
  [ C.Partida 1 "A" 2 0 "B"
  , C.Partida 1 "C" 1 1 "D"
  , C.Partida 2 "B" 0 3 "C"
  ]

main :: IO ()
main = do
  let a = C.linhaDoTime partidas "A"
      b = C.linhaDoTime partidas "B"
  assert "participou" (C.participou "A" (head partidas))
  assert "resultadoPara" (C.resultadoPara "B" (head partidas) == Just C.Derrota)
  assert "pontos" (C.pontos C.Vitoria == 3 && C.pontos C.Empate == 1 && C.pontos C.Derrota == 0)
  assert "times em ordem alfabética" (C.times partidas == ["A", "B", "C", "D"])
  assert "linha do time" (C.jogos a == 1 && C.pontosLinha a == 3 && C.saldo a == 2)
  assert "linha visitante" (C.jogos b == 2 && C.pontosLinha b == 0 && C.saldo b == (-5))
  assert "aproveitamento percentual" (C.aproveitamento a == 100)
  assert "ateRodada" (length (C.ateRodada 1 partidas) == 2)
  assert "mediaGols" (C.mediaGols partidas == Just (7 / 3))
  assert "mediaGols vazia" (C.mediaGols [] == Nothing)
  assert "maiorGoleada vazia" (C.maiorGoleada [] == Nothing)
  assert "classificação completa" (length (C.classificacao partidas) == 4)
  assert "desempate por saldo" (map C.time (C.classificacao [C.Partida 1 "A" 1 0 "B", C.Partida 1 "C" 3 0 "D"]) == ["C", "A", "B", "D"])
  assert "tabela formatada" ("POS | TIME" `prefixoDe` C.tabelaFormatada (C.classificacao partidas))
  assert "tabela rodada 3" (length (C.classificacao (C.ateRodada 3 partidas)) == 4)
  assert "entrada válida" (case C.lerCampeonato "1;A;2;0;B\n# comentário\n\n1;C;1;1;D" of Right xs -> length xs == 2; Left _ -> False)
  assert "placar inválido" (temNumeroInvalido (C.todosOsErros "1;A;dois;0;B"))
  assert "placar negativo" (temPlacarNegativo (C.todosOsErros "1;A;-1;0;B"))
  assert "campos incorretos" (temCamposInvalidos (C.todosOsErros "1;A;0;B"))
  assert "time contra si mesmo" (temMesmoTime (C.todosOsErros "1;A;0;1;A"))
  assert "conflito na rodada" (temTimeRepetido (C.todosOsErros "1;A;0;1;B\n1;A;1;0;C"))
  assert "todos os times repetidos" (quantidadeTimesRepetidos (C.todosOsErros "1;A;0;1;B\n1;A;1;0;C\n1;C;2;0;D") == 2)
  assert "coleta vários erros" (length (C.todosOsErros "1;A;dois;0;B\n1;C;-1;0;D\n1;E;0;F") >= 3)

prefixoDe :: String -> String -> Bool
prefixoDe prefixo texto = take (length prefixo) texto == prefixo

temNumeroInvalido :: [C.Erro] -> Bool
temNumeroInvalido = any eh
  where eh (C.NumeroInvalido _ _ _) = True; eh _ = False

temPlacarNegativo :: [C.Erro] -> Bool
temPlacarNegativo = any eh
  where eh (C.PlacarNegativo _ _ _) = True; eh _ = False

temCamposInvalidos :: [C.Erro] -> Bool
temCamposInvalidos = any eh
  where eh (C.CamposInvalidos _ _) = True; eh _ = False

temMesmoTime :: [C.Erro] -> Bool
temMesmoTime = any eh
  where eh (C.MesmoTime _ _) = True; eh _ = False

temTimeRepetido :: [C.Erro] -> Bool
temTimeRepetido = any eh
  where eh (C.TimeRepetidoNaRodada _ _ _) = True; eh _ = False

quantidadeTimesRepetidos :: [C.Erro] -> Int
quantidadeTimesRepetidos = length . filter eh
  where eh (C.TimeRepetidoNaRodada _ _ _) = True; eh _ = False
