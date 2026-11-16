# Campeonato Brasileiro em Haskell

Projeto da Camada 1 para modelagem e análise de partidas de futebol, usando somente módulos da biblioteca `base`.

## Formato oficial

Cada partida deve ocupar uma linha no formato:

```text
rodada;mandante;gols mandante;gols visitante;visitante
```

Linhas vazias e comentários iniciados por `#` são ignorados. A leitura valida campos ausentes, inteiros inválidos ou negativos, confronto do time contra si mesmo e repetição de um time na mesma rodada.

## Executar

```bash
ghc -dynamic -Wall main.hs -o campeonato
./campeonato
ghc -dynamic -Wall Test.hs main.hs -main-is Test -o testes
./testes
```

O programa também mantém um adaptador (`converterCSVHistorico`) para o CSV histórico que já estava na pasta `data/`.

## API principal

`Time`, `Resultado`, `Partida` e `Linha` formam o modelo do domínio. As funções puras incluem `participou`, `resultadoPara`, `times`, `pontos`, `linhaDoTime`, `classificacao`, `ateRodada`, `maiorGoleada`, `mediaGols`, `tabelaFormatada`, `separar`, `lerPartida`, `lerCampeonato` e `todosOsErros`. `lerPartida` e `lerCampeonato` usam `Either Erro` e param no primeiro erro; `todosOsErros` coleta todos os erros possíveis.
