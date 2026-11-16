# Decisões de implementação

## Modelo

`Partida` representa uma partida válida e `Linha` representa o acumulado de um time. O resultado de uma equipe é `Maybe Resultado`, pois um time que não participa da partida não deve receber um resultado.

## Acumulação

`linhaDoTime` usa `foldl` para acumular jogos, vitórias, empates, derrotas, gols e pontos. `times` também é construído por dobra, sem `Map` ou `Set`, mantendo a solução dentro de `base`.

## Erros

O parser retorna `Either Erro Partida` e `Either Erro [Partida]`; `traverse` faz a leitura parar no primeiro erro. A função `todosOsErros` percorre novamente a entrada para produzir um relatório completo. Cada construtor de `Erro` preserva o número da linha, inclusive conflitos entre equipes na mesma rodada.

## Entrada

O formato oficial é separado por ponto e vírgula. A função `converterCSVHistorico` existe apenas para reaproveitar o arquivo CSV legado e transforma cada registro histórico em uma linha oficial antes da validação.

## Classificação

A ordenação usa pontos, vitórias, saldo, gols pró e nome como desempate determinístico. O regulamento de 2019 ainda prevê confronto direto, cartões e sorteio; esses critérios não podem ser calculados porque os dados fornecidos não possuem confrontos separados para desempate, cartões ou sorteio. `aproveitamento` é o percentual dos pontos obtidos sobre o máximo possível, no intervalo de `0` a `100`.
