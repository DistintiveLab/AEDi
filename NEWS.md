# AEDi 0.6.7.9000 (2026-09-22)

## Catálogo de grupos declarado e reconciliado pelo próprio pacote

- `prepare_db()` passa a declarar o catálogo temático do DW (Eixos, Objetivos,
  Estratos PNAD, Eixo 1..7 e Objetivo 1..4) em `datagroup`/`group_parent`. Até
  aqui essas tabelas saíam vazias e as linhas do `aedidb` em produção tinham
  sido inseridas fora do pacote.
- Novo `atualizar_grupos_indicadores()` completa, sem apagar nada, os vínculos
  de `mdata_group` de todo indicador que siga a convenção de `orig_name`
  (`R/grupos_catalogo.R`); rodado no `aedidb`, levou `mdata_group` de 27 para
  66 linhas e cobriu os eixos 2, 4, 5, 6 e 7 e o Objetivo 4, que estavam sem
  vínculo. Vínculos fora da convenção são apenas relatados.
- `db_datawrite(grp = TRUE)` voltou a funcionar: lia `MAX(mdata_id)` de
  `datagroup` (coluna inexistente) e um objeto global de `prepare_db`, então
  abortava sempre; agora reaproveita o grupo do indicador quando ele já existe.

## Resumo da região pelo catálogo completo (7 eixos, 4 objetivos, PNAD)

- O catálogo da aba Região passa a ser montado pela convenção de
  `orig_name` (`painel_grupo_indicador()`/`painel_grupo_raiz()`) em vez da
  tabela `mdata_group`: ela existe no DW mas está populada só em parte
  (verificado: 7 dos 35 indicadores dos eixos), o que sumia com os eixos
  2, 4, 5, 6 e 7 e com o Objetivo 4 do resumo e do accordeon.
- A hierarquia agora traz as três raízes — Eixos (7 grupos × 4
  indicadores + composto), Objetivos (4 × 3 + composto) e Estratos PNAD
  (7 `pnadc` + 7 compostos) — com `data_class_id` marcando os compostos.
  Variações de trabalho (`*_via_aedi`, `*_v0`) e séries de apoio ficam
  fora do que o painel publica.
- O card passou a se chamar "Resumo da região" e mostra o nível
  territorial corrente e a localidade escolhida; os chips vêm de
  `painel_resumo_grupos()`, uma função pura que pega o valor mais recente
  de cada composto do catálogo na localidade.
- As caixas de indicador, nível e localidade viraram `selectizeInput`
  pesquisável (`painel_opcoes_select()`): o selectize renderiza no máximo
  1000 itens por padrão e a lista de municípios (5.716 com dados) era
  cortada antes do fim — a busca por parte do nome continua funcionando.

# AEDi 0.6.7 (2026-09-22)

## Grafo de dependências no orquestrador (fase 1)

Scripts que derivam séries de outras (DW→DW: compostos, sincronizações
`_via_aedi`→builder, diferenciais) agora podem ser bloqueados quando o
insumo está menos fresco que a série própria — o cenário que zerou o
objetivo2_3 em 2025 (`massa_salarial_municipal` rodado com o popmun sem
2025-07-01; ver `pndr_coord/bug_obj23_zeros_2025_2026-09-22.md`).

- O grafo vive no DW: coluna `controle_execucao.dependencias_json`
  (JSONB, dormante desde a criação da tabela), no formato
  `{"series_proprias": [...], "deps": [{"serie","regua","acao"}]}`.
  `definir_dependencias()` grava via UPSERT; `carregar_dependencias()`
  carrega a semente versionada `<raiz>/coleta/dependencias.csv`, que
  segue como fallback do runtime quando a linha do DW não tem grafo.
- `executar_script_coleta()` verifica as dependências logo após a
  novidade de fonte (C5): dependência em ano anterior ao da série própria
  (régua por ANO, nunca data completa — popmun é -07-01) com
  `acao="pular"` registra skip ok no controle e pula o script;
  `acao="avisar"` apenas loga em warn e executa (lags crônicos legítimos:
  sust4/citec4/infra1 em 2024). Em dúvida (grafo ausente, DW fora, série
  própria ausente = primeira carga), executa — mesma política do C5.
- Séries próprias detectadas do texto do script (literais de
  `gravar_serie_dw()`), com override por `serie_propria` no manifesto
  (compostos gravam via variável e não são detectados pelo parse).
- Na fase 1 as réguas "proprio" e "fonte" coincidem (comparam com o ano
  da série própria); a régua "fonte" plena (ano-alvo da fonte primária,
  que pegaria o bug original) depende de reconhecer RAIS em
  `verificar_novidade_fonte()` e fica para a fase 1.5.
- Semente inicial do `pndr_dashboard` em `coleta/dependencias.csv`
  (compostos_recalc_dw, massa_salarial_municipal,
  objetivo1_diferenciais_recalc, objetivo4_1_via_aedi_recalc,
  primazia_populacional_estadual, sincroniza_via_aedi_local).
- `controle_preparar()` agora garante a coluna `dependencias_json` em
  bancos criados antes dela (`ALTER ... ADD COLUMN IF NOT EXISTS`).

# AEDi 0.6.6 (2026-09-22)

## Correção do bug de cobertura municipal, segunda ordem (local_id sombreado por geoloc de agregado)

Em `gravar_serie_dw()`, o lookup ainda priorizava o `geoloc_id` como
texto antes do próprio `local_id`. Derivações DW→DW (padrão A5b: séries
derivadas, compostos) repassam `local_id` — e os ids pequenos dos
municípios collidem com geoloc_ids de agregados (1 = Alta Floresta
D'oeste vs 1 = Norte; 2 = Ariquemes vs 2 = Nordeste; 53 = Acrelândia
vs 53 = DF etc.), de modo que essas séries eram gravadas em região/UF/DF
no lugar do município. A prioridade agora é prefixo IBGE 6d (RAIS) >
`local_id` > `geoloc_id` completo; séries agregadas devem ser passadas
por `local_id`.

# AEDi 0.6.5 (2026-09-22)

## Conexão de source-time não vaza mais para o namespace

- `R/create_extend_geogroup_view.R` conectava ao banco remoto (`tdbname`)
  em source-time e deixava o objeto `con` no namespace; sob
  `pkgload::load_all()` (desenvolvimento), scripts A5b com guard
  `if (!exists("con"))` herdavam essa conexão remota e liam o banco
  errado sem qualquer erro. A conexão agora é criada sob demanda dentro
  de `criar_recortes_geograficos()` e desconectada ao final.

# AEDi 0.6.4 (2026-09-22)

## Correção do bug de cobertura municipal (lookup 6d sombreado por RGINT)

Em `gravar_serie_dw()`, o lookup de códigos de local priorizava o
`geoloc_id` como texto antes do prefixo IBGE de 6 dígitos. Como o
`geoloc_id` das Regiões Imediatas também tem 6 dígitos, 63 municípios
(cujo código 6d coincide com o geoloc de uma RGINT) tinham suas séries
municipais gravadas no local da RGINT — eram exatamente os municípios
"primeiros no sequencial por UF" que faltavam na cobertura do painel.

- O prefixo 6d (RAIS) agora tem prioridade sobre o `geoloc_id` como
  texto; geoloc 7d e o próprio `local_id` continuam resolvendo como
  antes (verificado contra o DW: exatamente 63 entradas mudam).
- O lookup foi extraído para `montar_lookup_locais()` (interna), com
  teste de regressão (`test-montar_lookup_locais.R`).
- Séries já gravadas nos locais errados **não** são corrigidas
  automaticamente: os scripts vivos se autocuram no próximo run
  (`replace = TRUE`); o remapeamento das demais é documentado em
  `pndr_coord/bug_RGINT_2026-09-22.md`.

# AEDi 0.6.3 (2026-09-21)

Marco do orquestrador em produção: primeiro lote completo do
`pndr_dashboard` encerrado com a família era-RAIS resolvida de ponta a
ponta (contrato de eras centralizado no pacote **raisqlr** 0.1.0) e 13
scripts aposentados via `.R.ignore`. Sem mudanças de código no AEDi por
parte desta entrada — ela marca o ponto em que o pipeline externo
consumidor estabilizou sobre o AEDi 0.6.2; a versão também embute o
trabalho do painel registrado na entrada 0.6.2.9000 abaixo.

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

# AEDi 0.6.2.9000 (2026-09-20)

Painel de indicadores contra a lentidão do DW remoto (handshake ~4s e
consultas agregadas de segundos, medidas contra o `aedidb` remoto do
`pndr_dashboard`), mais blindagem de `mdata_id` inválido.

## Globo com as delimitações do IBGE do nível territorial

- As camadas do globo passam a seguir o nível escolhido na aba Região:
  níveis leves desenham as próprias delimitações do IBGE lidas do banco de
  dados (`painel_geo_nivel()`, simplificadas no SQL e limitadas a 700
  feições), enquanto níveis pesados (ex.: município) mantêm a base de UFs,
  destacam a localidade escolhida (`painel_geo_local()`) sobre a UF pai
  (`painel_geo_pai_uf()`) e animam o zoom até o estado inteiro caber no
  globo.
- Clique contextual: clicar em uma área com dados escolhe localidade do
  nível corrente; no modo UF, o clique leva à localidade com mais pontos do
  indicador naquela UF (`painel_local_top_uf()`) e a pintura de
  disponibilidade usa as UFs com dados no nível corrente
  (`painel_ufs_com_dados()`).
- Zoom máximo ampliado de 6× para 256×, com precisão dinâmica do d3 e passo
  de rotação escalado para manter a suavidade em zoom alto.
- Todos os novos acessores de geometria têm versão `_cache` (TTL de
  catálogo/geo conforme o tipo de leitura).

## Globo com o mundo inteiro e zoom (aba Região)

- `inst/painel/painel-globe.js` agora desenha o mundo inteiro atrás das
  UFs: contornos dos países do pacote `maps`, convertidos uma única vez
  por `data-raw/painel_mundo.R` no asset `inst/painel/painel-mundo.geojson`
  (134 KB, 253 feições simplificadas), servido pelo resource path
  `painel_recursos` e buscado pelo cliente com falha silenciosa.
- Zoom no estilo Google Earth: botões Aproximar/Afastar e roda do mouse
  (com `preventDefault`), clamped entre 1× e 6× sobre o raio base; a
  projeção continua ortográfica arrastável.
- A aba Região abre por padrão no nível municipal quando disponível
  (`painel_nivel_default()`; fallback UF).

## Resumo da localidade na aba Região (padrão labourvaluesdatapanel)

- Novo card "Resumo da localidade" acima da série: chips com o último
  valor de cada indicador composto (`mdata_exts.data_class_id = 4`)
  vinculado a cada objetivo (raiz "Objetivos" do `datagroup`), com o ano
  de referência; sem hierarquia ou sem compostos, mensagem explicativa.
- O botão "Mostrar mais" expande um accordeon aninhado raiz (Eixos,
  Objetivos) > grupo > indicador com mini-gráficos da série na
  localidade corrente; os `plotOutput`s são registrados uma única vez e
  filtram os valores por id.
- Leituras novas em `R/painel_dw.R` (+ cópia no esqueleto):
  `painel_hierarquia()`, `painel_compostos()` e
  `painel_valores_local_todos()` (uma única leitura alimenta resumo e
  mini-gráficos), com acessores `_cache` (catálogo 7d / valores 24h) e
  guards de `local_id` inválido retornando vazio sem tocar no cache.

## Mapa: slider, animação e legenda

- Slider de ano ocupa ~40% da barra em telas largas (era flex de 300px)
  e continua integral no mobile; a animação passou a 7 segundos por ano
  (`animationOptions(interval = 7000)`), dando tempo de pintar o ano e
  de o usuário ler o mapa.
- Caixa de legenda colapsável deixa de esticar em tela grande:
  `width: max-content` com teto `min(360px, calc(100vw - 48px))` e
  tabela `width: auto`.

## "No DW do AEDi" → "no banco de dados do painel"

- Todo texto visível ao usuário que mencionava o DW (títulos e lead do
  Sobre, modal e validação do Mapa, nota e validações da Região, rodapé,
  subtítulo default do topbar e da marca, launcher gerado e
  app.R/README.md do esqueleto) agora diz "banco de dados do painel".
  Identificadores internos e comentários dev-facing mantêm "DW".

## Cache de duas camadas e agregação por ano

- **Novo `R/painel_cache.R`** (copiado para o esqueleto e listado em
  `deploy_panel()`/`atualizar_painel()`): leituras do DW passam por
  memória do processo (compartilhada entre sessões Shiny) + RDS em disco
  em `cache/` relativo à app, sobrevivendo a reinícios e redeploys.
  Chaves incluem `host`+`dbname` (DW local e remoto nunca dividem
  entradas); TTLs: geometrias 30d, catálogo 7d, valores 24h.
  `painel_cache_limpar()` descarta tudo (rodar após ETL) e
  `painel_sem_cache=1` desativa.
- **Acessores `painel_*_cache()`** em `R/painel_dw.R`: `painel_com_con()`
  abre conexão só na falha de cache; os módulos (Região, Mapa, Globo)
  não tocam mais `painel_con()` diretamente. A partida da app, que
  abria 3 conexões sequenciais (~50s a cada reload no remoto), cai para
  zero conexões com cache quente.
- **`painel_valores_ano()`**: o mapa agrega no próprio SQL (`DISTINCT
  ON` + faixa `make_date`, amigável ao índice da PK) — último `refdate`
  de cada localidade no ano, ~5,6 mil linhas em vez das ~73 mil do
  indicador inteiro (verificado equivalente à derivação em R).
  `painel_valores_local()` e `painel_anos()` fazem o mesmo para a série
  da aba Região e os limites do slider.
- **Spinner em CSS puro** (`inst/painel/painel.css`): toda saída
  `recalculating` ganha overlay com círculo giratório, no estilo Gov.br
  (usa as variáveis da paleta), sem dependência nova.

## Basemap do mapa: Carto com chave ou fundo neutro

- **Novo `R/painel_basemap.R`** (pacote e esqueleto): o Carto passou a
  exigir chave de API nos tiles. Com `CARTO_API_KEY` no ambiente, o mapa
  usa os rastertiles voyager com a chave anexada como `?key=` nas
  chamadas (`subdomains abcd`, `maxZoom 20`). Sem chave, vale o padrão
  do labourvaluesdatapanel: fundo neutro vetorial sem tiles, com o
  contorno das UFs (malha do IBGE, lida do próprio DW) em um pane acima
  da camada municipal e fundo cinza no CSS — nenhuma dependência
  externa. `PAINEL_BASEMAP=carto|neutro` força a opção; `carto` sem
  chave cai no neutro com aviso.

## Robustez

- `mdata_id`/`nivel_id`/`local_id` inválidos (`"NA"`, `""`, ausentes —
  possíveis com `selectizeInput(server = TRUE)`) retornam vazio em vez de
  estourar erro de SQL; seleções iniciais tratam `mdata` vazia; aba
  Mapa/Região explicam DW sem indicadores via `validate()`.

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
