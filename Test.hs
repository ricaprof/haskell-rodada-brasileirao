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
  let a = C.linhaDoTime "A" partidas
      b = C.linhaDoTime "B" partidas
  assert "participou" (C.participou "A" (head partidas))
  assert "resultadoPara" (C.resultadoPara "B" (head partidas) == Just C.Derrota)
  assert "pontos" (C.pontos C.Vitoria == 3 && C.pontos C.Empate == 1 && C.pontos C.Derrota == 0)
  assert "linha do time" (C.jogos a == 1 && C.pontosLinha a == 3 && C.saldo a == 2)
  assert "linha visitante" (C.jogos b == 2 && C.pontosLinha b == 0 && C.saldo b == (-5))
  assert "ateRodada" (length (C.ateRodada 1 partidas) == 2)
  assert "mediaGols" (C.mediaGols partidas == 7 / 3)
  assert "entrada válida" (case C.lerCampeonato "1;A;2;0;B\n# comentário\n\n1;C;1;1;D" of Right xs -> length xs == 2; Left _ -> False)
  assert "placar inválido" (not (null (C.todosOsErros "1;A;dois;0;B")))
  assert "placar negativo" (not (null (C.todosOsErros "1;A;-1;0;B")))
  assert "campos incorretos" (not (null (C.todosOsErros "1;A;0;B")))
  assert "time contra si mesmo" (not (null (C.todosOsErros "1;A;0;1;A")))
  assert "conflito na rodada" (not (null (C.todosOsErros "1;A;0;1;B\n1;A;1;0;C")))
