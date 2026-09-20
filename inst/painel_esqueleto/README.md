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
| `R/painel_dw.R` | Camada de dados: conexão e leituras do DW (`painel_con()` e `painel_*`). |
| `R/branding.R` | Marca configurável por variáveis de ambiente. |
| `www/` | CSS/JS do painel (mapa incremental, globo) e logo. |
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

## Credenciais do DW

A conexão usa as variáveis de ambiente `user`, `password`, `host` e
`dbname` (defaults: aedi@127.0.0.1/aedidb), lidas por `painel_con()` em
`R/painel_dw.R`. Em hospedagem, configure-as no painel da plataforma ou
no `.Renviron` lido pelo processo do Shiny.

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
