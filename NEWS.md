# AEDi 0.6.3 (2026-09-21)

Marco do orquestrador em produção: primeiro lote completo do
`pndr_dashboard` encerrado com a família era-RAIS resolvida de ponta a
ponta (contrato de eras centralizado no pacote **raisqlr** 0.1.0) e 13
scripts aposentados via `.R.ignore`. Sem mudanças de código no AEDi —
esta versão apenas marca o ponto em que o pipeline externo consumidor
estabilizou sobre o AEDi 0.6.2.

- **Placar do lote (44 scripts): 27 ✓ / 17 ✗**, encerrado com a família
  era-RAIS (7 scripts) resolvida via `raisqlr` 0.1.0 (7/7 ✓ no re-run)
  e 13 marcadores `.R.ignore` (no-ops, rascunhos, utilitários legados,
  insumo manual pendente e `gastos_tributarios_municipio`, que
  downdata sem `AEDI_SCRIPT_ARGS`); `listar_scripts_coleta()` devolve
  34 ativos.
- **Séries RAIS estendidas a 2000-2025** nos indicadores do
  `pndr_dashboard`: os 7 scripts era-RAIS passaram a montar o SQL por
  era com `raisqlr::cnae_equivalentes()`, `raisqlr::rais_coluna()` e
  `raisqlr::rais_divisor()`, absorvendo as mudanças de esquema do
  `mte_rais` (CNAE 95→2.0, porte/tamanho, CBO-94) sem lógica de era
  nos próprios scripts.

# AEDi 0.6.2 (2026-09-21)

- **latex2r → latexr** (rename CRAN; `latex2r` foi arquivado): Imports
  trocado para `latexr` e a conversão de equações usa
  `latexr::latex2r()` (a função mantém o nome no fork). Os
  `@importFrom` redundantes caíram — o símbolo re-exportado por
  `shinymath` não é mais importado, eliminando o note de "replacing
  previous import" no load. Nota: `shinymath` ainda depende de
  `latex2r`, que segue instalado; quando o shinymath migrar para o
  `latexr`, nenhuma mudança será necessária aqui.

# AEDi 0.6.1 (2026-09-21)

Robustez do orquestrador `atualizar_indicadores()` validada num lote
completo de 44 scripts do `pndr_dashboard` (ambiente real de produção),
e migração da dependência educabR → edubr (rename CRAN).

## Correções no orquestrador

- **Bookkeeping do lote protegido**: o registro de contabilidade ao fim
  de cada script (tabelas `controle_execucao`) roda em `tryCatch` — um
  crash aí não aborta mais o lote inteiro.
- **`commandArgs()` mascarado por script**: scripts escritos para
  Rscript standalone liam os argumentos do processo do orquestrador (ex.:
  índice do batch interpretado como "ano 4"). Agora cada script roda como
  se sem argumentos; argumentos pontuais vêm da env var
  `AEDI_SCRIPT_ARGS` (ex.: `AEDI_SCRIPT_ARGS="2025"`).
- **Helper de sessão `somasna` provisionado**: padrão A5b histórico,
  definido "na sessão" por scripts fora do lote e usado sem definição por
  `indicadores_agregado_uf` e variantes de população.
- **`AEDi` e `edubr` nos pacotes candidatos** do lote: resolve chamadas
  bare como `gravar_serie_dw()` nos scripts de coleta.
- **`.dado_edubr()` corrigido**: `data(list = nome)` em vez de
  `data(nome)` — antes buscava um dataset literal chamado "nome" e
  deixava o módulo INEP com catálogos vazios.

## Rename de dependência

- **educabR → edubr** (DESCRIPTION, `upload_inep`, `verifica_fonte`):
  `metainep`/`metaideb` são datasets lazy-data do edubr (carregados via
  `data()`), não exports; `verifica_fonte` ainda aceita chamadas
  `educabR::le_*` em scripts gerados antes do rename.

# AEDi 0.6.0 (2026-09-20)

O painel de indicadores do DW deixa de ser só uma launcher sobre o pacote:
`deploy_panel()` agora gera, por padrão, um **esqueleto minimamente
funcional e autônomo** — cópia que cada projeto passa a possuir e adapta
livremente (abas, layout, marca), sem depender do AEDi em execução.

## Novidades

- **`deploy_panel()` gera esqueleto adaptável (novo padrão)**: além do
  `app.R`, materializa `R/` (módulos das abas, blocos de UI componíveis,
  camada de dados `painel_dw.R`, marca configurável em `branding.R`),
  `www/` (CSS/JS/logo), `README.md` de instruções e
  `esqueleto_manifest.json` com a versão geradora e o SHA-256 de cada
  arquivo (auditoria de mudanças locais). O modo launcher continua
  disponível com `esqueleto = FALSE` — na VPS, onde atualizações do
  painel chegam com o reinstall do pacote, é ele que segue valendo.
- **`atualizar_painel()`**: absorve melhorias do AEDi num esqueleto já
  gerado **preservando edições locais** — pelo manifest, arquivos
  intactos são atualizados, modificados ficam intocados e reportados
  (com o hash upstream esperado no manifest, a divergência segue
  detectável na próxima rodada); `forcar = TRUE` repõe tudo pelo
  upstream. Arquivos novos são adicionados; retirados do esqueleto,
  apenas reportados, nunca apagados.
- **UI do painel em blocos componíveis** (`painel_logo_src()`,
  `painel_recursos()`, `painel_topbar()`, `painel_abas()`,
  `painel_rodape()`): launcher e esqueleto compartilham exatamente os
  mesmos fontes, sem duplicação manual — teste de sincronia guarda a
  igualança dos templates com `R/` do pacote.
- **Marca configurável por variáveis de ambiente** (`painel_titulo`,
  `painel_subtitulo`, `painel_paleta`, `painel_contato`) via
  `painel_brand_*()`.
- Esqueleto aplicado no consumidor real: o `painel/` do pndr_dashboard
  migrou da launcher para o esqueleto (título preservado).

## Correções

- O `app.R` do esqueleto anexa `library(shiny)` antes do sourcing dos
  módulos — usavam `NS()`/`tagList()` sem prefixo, que só resolviam
  dentro do namespace do pacote.
- O parse-check pós-geração buscava os arquivos de `R/` no diretório
  raiz da app em vez de `R/`.
- Testes do manifest endurecidos (hashes verificados com `vapply`, não
  mais vacuos).

# AEDi 0.4.3 (2026-09-17)

Correções de robustez do lote em clones novos (ex.: VPS) e em scripts com
construtos que quebravam a varredura de pacotes.

## Correções

- **`db_datawrite()` com defaults de conexão**: conectava com
  `Sys.getenv("user")`... **sem** defaults — no lote sob outra raiz (ex.:
  pndr_dashboard, cujo `.Renviron` não define `user`/`password`/`host`/
  `dbname`) caía num socket unix do usuário OS (banco sem schema do aedidb,
  erro "relation \"mdata\" does not exist"). Agora usa o mesmo padrão de
  defaults de `gravar_serie_dw()`/scripts (`aedi@127.0.0.1/aedidb`); no app
  as vars sempre estão definidas, sem mudança de comportamento.

# AEDi 0.4.2 (2026-09-17)

Correções de robustez do lote em clones novos (ex.: VPS) e em scripts com
construtos que quebravam a varredura de pacotes.

## Correções

- **Criação preventiva dos diretórios de cache**: `write_csv()` (readr)
  não cria diretório-pai, então em clones sem os caches históricos (caso
  da VPS `/dw`) todo script que grava em `coleta/<nome>/...` falhava com
  "Cannot open file for writing". O lote agora cria, antes de executar o
  script, os diretórios `coleta/cache/...` citados no texto do script
  (detecção por regex sobre `readLines()`, que tolera scripts com
  subscript vazio `df[i, ]` e até scripts que não parseiam). Falha de
  criação (permissão) vira erro claro com o caminho.
- **cwd = raiz durante o script**: scripts leem/escrevem com caminhos
  relativos à raiz; agora vale mesmo com o lote disparado de outro
  diretório via `atualizar_indicadores(raiz = ...)`.
- **`.heads_bare()` blindado**: o objeto "missing" gerado por subscritos
  vazios (`df[i, ]`) derrubava a detecção de pacotes ("argument 'a' is
  missing") em ~11 scripts ativos (ex.: `datasus_popmun_update`,
  `infra1_sinisa`, `rais_vinculos_s38`); cada ramo da varredura agora é
  protegido individualmente.

# AEDi 0.4.1 (2026-09-17)

Orquestrador multi-projeto: o AEDi vira o backend abstrato de
atualização/agendamento e o projeto de painel (ex.: pndr_dashboard) passa a
ser o dono concreto do lote `coleta/`.

## Novidades

- **Coluna `projeto`** nas tabelas `controle_execucao`,
  `controle_execucao_historico` e `versoes_carga` do aedidb: chave =
  `basename()` da raiz do lote (ex.: "AEDi", "pndr_dashboard"). A PK de
  `controle_execucao` passa a ser `(projeto, nome_script)`. Migração
  idempotente dentro de `controle_preparar()` — registros anteriores
  pertencem ao projeto "AEDi" (backfill do DEFAULT).
- `ler_controle()`, `controle_inicio()`, `controle_fim()` e
  `versao_carga_inicio()` ganham o parâmetro `projeto` (default "AEDi",
  back-compatível).
- `atualizar_indicadores()` ganha o parâmetro `raiz` (default: cwd com
  `coleta/`), propagado para o `.Renviron`, `listar_scripts_coleta()` e a
  execução de cada script; o projeto do controle deriva da raiz.
- A aba "Atualização" filtra o controle pelo projeto da raiz em vez de
  mostrar registros de todos os lotes.
- Lote próprio do AEDi aposentado: os 46 scripts de `coleta/` do repo
  ficaram com `.R.ignore`; a fonte executável agora é o `coleta/` do
  pndr_dashboard (46 cópias sincronizadas, `objetivo4_3_diversificacao`
  aposentado nos dois repos — reproduzia perda de dados ao regravar só UF).

## Correções

- `hash_coleta_csv()` em `executar_script_coleta()` usava `.aedi_raiz()`
  em vez da raiz do projeto em execução (hash errado quando a raiz difere
  do cwd).

# AEDi 0.4.0 (2026-09-17)

Redesign do painel de indicadores, aproveitando a estrutura visual do
labourvaluesdatapanel (topbar com marca, abas realçadas, página "Sobre"
editorial).

## Novidades

- **Aba "Região"** (antes "Séries"): seletor de nível territorial — região,
  UF, região geográfica intermediária, microrregião, região imediata ou
  município — carrega as localidades daquele nível com dados no DW e abre a
  série na localidade de maior cobertura do indicador. Municípios ganham a
  sigla da UF para desambiguar nomes repetidos.
- **Duas paletas** trocáveis no botão do topo (escolha salva no navegador):
  **Gov.br** (padrão, azul `#1351B4`, tipografia Rawline) ou **preto e
  branco** com toques do roxo da Distintive (`#78529D`, cor dominante do
  logotipo). A cor da série acompanha a paleta ativa. Novo parâmetro
  `paleta` em `panel_app()`/`run_panel()`/`deploy_panel()`.
- **Aba "Sobre"** minimalista: apresentação do painel, cartão pessoal
  (Rodrigo Emmanuel Santana Borges, rodrigo@borges.net.br) e
  agradecimento ao apoio material e financeiro da Distintive
  (distintive.com.br).
- Topbar com marca e botão de paleta, abas com sublinhado realçado,
  rodapé, indicador de "carregando", navegação por teclado/foco visível e
  layout responsivo (menu colapsável no celular).
- Logo do painel funciona também na app standalone: o fallback aponta para
  o arquivo embutido no pacote (`painel_logo_src()`), não mais para `www/`
  do app completo.

## Remoções

- Módulo `mod_panel_series` (substituído por `mod_panel_regiao`, com o
  seletor de nível territorial).

# AEDi 0.3.0 (2026-09-17)

Deploy básico do painel de indicadores dentro do projeto que chamar.

## Novidades

- **`deploy_panel()`**: materializa uma app Shiny autônoma do painel em um
  subdiretório do projeto corrente (default `painel/`) — `app.R` de uma
  linha + `README.md` com instruções de execução, hospedagem (Shiny Server
  ou `rsconnect::deployApp()`) e credenciais do DW. Diferente de
  `run_panel()`, que só lança o painel na sessão atual.
- **`panel_app()`** (exportada): constrói e devolve o objeto `shinyApp` do
  painel — motor comum de `run_panel()` e da app gerada por
  `deploy_panel()`, utilizável como última expressão de um `app.R`
  hospedável.

# AEDi 0.2.0 (2026-09-17)

Correções no lote de atualização para ambientes de produção (VPS), onde o
painel dispara a coleta em subprocesso.

## Correções e robustez

- Lote de coleta agora carrega o `.Renviron` da raiz do projeto na entrada
  de `executar_script_coleta()`/`atualizar_indicadores()`: o subprocesso
  callr do painel lê apenas o `~/.Renviron` do usuário, o que deixava as
  credenciais do banco RAIS (e outras) invisíveis mesmo com o arquivo
  correto na raiz.
- Erro de script de coleta passa a ser anotado com os objetos de sessão
  que o lote não conseguiu fornecer (con/mdr/rais/locgeoloc) e o motivo de
  cada um (ex.: variáveis ausentes no ambiente, banco indisponível).
- Conexão RAIS aceita `dbrais` como dbname com fallback para `mte_rais`,
  cobrindo as duas convenções de variáveis do repositório.

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
