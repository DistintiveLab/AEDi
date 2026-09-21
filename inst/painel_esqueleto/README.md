# %%TITULO%%

Esqueleto de painel de indicadores gerado por `AEDi::deploy_panel()`
(AEDi %%VERSAO%%, %%DATA%%). **Este projeto é dono deste código** —
adaptar, criar e mudar elementos é o esperado:

| Arquivo | Papel |
|---|---|
| `app.R` | Carrega `R/*.R` e sobe a app (`shiny::runApp("painel")`). |
| `R/app_ui.R` / `R/app_server.R` | Composição da UI e do server — por onde começar a adaptar. |
| `R/mod_panel_*.R` | Uma aba por módulo Shiny (`moduleServer`): Região, Mapa, Sobre (+ globo). |
| `R/painel_ui.R` | Blocos reutilizáveis: topbar, abas (`painel_abas()`), rodapé, recursos. |
| `R/painel_dw.R` | Camada de dados: conexão e leituras do banco de dados (`painel_con()` e `painel_*`). |
| `R/painel_cache.R` | Cache de duas camadas (memória + disco) das leituras do banco de dados — ver seção Cache. |
| `R/painel_basemap.R` | Basemap do mapa: tiles Carto com `CARTO_API_KEY` ou fundo neutro vetorial — ver seção Basemap. |
| `R/branding.R` | Marca configurável por variáveis de ambiente. |
| `www/` | CSS/JS do painel (mapa incremental, globo com zoom e delimitações do IBGE por nível territorial, resumo da aba Região), logo e `painel-mundo.geojson` (contorno dos países no globo). |
| `esqueleto_manifest.json` | Versão do AEDi gerador + SHA-256 de cada arquivo (auditoria de mudanças locais). |

## Rodar localmente

```r
shiny::runApp("painel")
```

## Hospedar

- Shiny Server: aponte a configuração do site para este diretório;
- shinyapps.io / Posit Connect:

```r
rsconnect::deployApp("painel")
```

## Credenciais do banco de dados

A conexão usa as variáveis de ambiente `user`, `password`, `host` e
`dbname` (defaults: aedi@127.0.0.1/aedidb), lidas por `painel_con()` em
`R/painel_dw.R`. Em hospedagem, configure-as no painel da plataforma ou
no `.Renviron` lido pelo processo do Shiny.

## Cache de leituras do banco de dados

O handshake com o banco de dados remoto é lento e as consultas agregadas levam
segundos, então as leituras passam por um cache de duas camadas em
`R/painel_cache.R` — memória do processo (compartilhada entre sessões
Shiny) e RDS em disco no diretório `cache/` (sobrevive a reinícios do
processo e a redeploys). As chaves incluem `host` + `dbname`, então banco
local e remoto nunca dividem entradas.

- Validade: geometrias 30 dias, catálogo (indicadores, níveis e
  localidades) 7 dias, valores 24 horas;
- Após rodar um ETL que atualiza o banco de dados: `painel_cache_limpar()` no console
  do projeto, ou aguarde o TTL expirar;
- `painel_sem_cache=1` no ambiente desativa o cache (leitura direta);
- Em hospedagem, `cache/` é criado em tempo de execução no diretório da
  app (a primeira visita preenche; as seguintes servem do disco).

Adicione `cache/` ao `.gitignore` do projeto: é dado derivado, não fonte.

## Basemap do mapa

O Carto passou a exigir chave de API nos tiles, então o fundo do mapa
municipal é decidido por variáveis de ambiente (sem editar código):

- `CARTO_API_KEY` definida: rastertiles voyager do Carto, com a chave
  anexada como `?key=` nas chamadas de tiles (a chave fica embutida na
  página, como em qualquer basemap servido por URL);
- sem chave: fundo neutro vetorial sem tiles, no padrão do
  labourvaluesdatapanel — malha municipal + contorno das UFs (malha do
  IBGE lida do banco de dados) sobre fundo cinza claro; nenhuma chamada externa de
  tiles;
- `PAINEL_BASEMAP=carto|neutro` força uma das opções (`carto` sem chave
  cai no neutro, com aviso no console).

## Aba Região: resumo e accordeon

A aba abre em um resumo da localidade — último valor de cada indicador
composto, agrupado por objetivo — com o nível municipal selecionado por
padrão. O botão "Mostrar mais" expande um accordeon com todos os
indicadores agrupados por eixo e por objetivo, cada um com um mini-gráfico
da série histórica.

## Marca e configuração (`.Renviron`, sem editar código)

- `painel_titulo`, `painel_subtitulo` — textos do topbar;
- `painel_paleta` — paleta inicial (`govbr` ou `pb`);
- `painel_contato` — contato do rodapé, campos `"nome|telefone|email"`;
- `aedi_logo` — logo (URL, caminho local ou nome de arquivo em `www/`).

## Dependências

- Pacotes do CRAN usados pelos módulos (shiny, leaflet, plotly, DBI,
  RPostgres, dplyr, ...);
- `shinyGovBRstyle` (GitHub):
  `remotes::install_github("DistintiveLab/shinyGovBRstyle")`.

O AEDi **não** é dependência em tempo de execução: o esqueleto é autônomo.

## Atualizar a partir de um AEDi novo

O esqueleto é uma cópia: atualizações do AEDi **não** se propagam
sozinhas. O `esqueleto_manifest.json` registra a versão geradora e o
hash de cada arquivo para você auditar o que mudou localmente antes de
regenerar (`AEDi::deploy_panel(..., sobrescrever = TRUE)` substitui os
arquivos existentes).
