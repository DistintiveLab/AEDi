# AEDi 0.1.0 (2026-09-17)

Primeira versão com o ciclo de atualização automatizado dos indicadores e o
painel público standalone, além das atualizações e correções de dados 2025 no
DW (local e remoto).

## Novidades

- **Agendamento e painel de atualização (fase C)**: tabelas de controle de
  execução no DW, `atualizar_indicadores()` sobre os scripts `coleta/*.R`,
  aba "Atualização" na app com execução via callr e `setup_agendamento.R`
  (cronR/taskscheduleR).
- **`run_panel()`**: painel de indicadores standalone (mapa coroplético +
  séries) lendo direto do DW, sem a app completa de construção de indicadores.
- Pré-verificação de novidades pula coleta já atualizada; painel de
  atualização destaca indicadores nunca executados, botão por indicador e
  indicador de execução (sem shinybusy).
- Contatos do header e organização da sidebar configuráveis via `.Renviron`.

## Correções e robustez

- Instalação e carregamento do pacote funcionam sem o banco RAIS e fora do
  diretório do projeto; o painel não derruba o processo quando o banco
  principal falha.
- `gravar_serie_dw()` mais robusto; citec4 2024 gravado com 0-fill municipal.
- Restauração da série municipal do objetivo4_3 (mdata 54, 2013–2025,
  municípios + UFs) nos dois DWs, perdida por um script que gravava só UF;
  o restaurador também cria as matviews `objetivo4_*` que faltarem.
- Scripts de atualização do DW: compostos 2025, educ4 (Ideb), infra4
  (SICONFI/DCA), transferências da União municipais e gastos tributários
  municipalizados (2022).
