# Decisões de implementação

## Modelo

`Partida` representa uma partida válida e `Linha` representa o acumulado de um time. O resultado de uma equipe é `Maybe Resultado`, pois um time que não participa da partida não deve receber um resultado.

## Acumulação

`linhaDoTime` usa `foldl` para acumular jogos, vitórias, empates, derrotas, gols e pontos. `times` também é construído por dobra, sem `Map` ou `Set`, mantendo a solução dentro de `base`.

## Erros

O parser retorna `Either [Erro] Partida` e `Either [Erro] [Partida]`, permitindo que a chamada preserve todos os problemas detectados. O número da linha é guardado em cada erro; conflitos entre equipes na mesma rodada usam linha `0` porque são erros relacionais do conjunto, não de uma linha isolada.

## Entrada

O formato oficial é separado por ponto e vírgula. A função `converterCSVHistorico` existe apenas para reaproveitar o arquivo CSV legado e transforma cada registro histórico em uma linha oficial antes da validação.

## Classificação

A ordenação usa pontos, vitórias, saldo e nome como desempate determinístico. `aproveitamento` é a fração dos pontos obtidos sobre o máximo possível, no intervalo de `0` a `1`.
