# AGENTS.md

Guidance for AI agents working in the **AEDi** repository.

## What This Project Is

AEDi is an R package that bundles an interactive **Shiny dashboard** (built on the
[`golem`](https://thinkr-open.github.io/golem/) framework) for **Análise Exploratória
de Dados e Indicadores** — importing Brazilian public-data sources, storing them in a
relational database, and deriving composite indicators from them.

- **Language/locale:** All UI text, code comments, and user-facing strings are in
  **Portuguese (pt-BR)**. `DESCRIPTION` declares `Language: pt-BR`. Match this when
  editing user-facing strings.
- **Lifecycle:** `experimental` / WIP. Originated from the `owEDA`/`owEDA` package
  (legacy entry points still reference `owEDA:::app_server` in `inst/app/`).
- **License:** MIT.

## Essential Commands

The package follows the standard `golem` + `devtools` workflow (see `devhist.R` for
the canonical setup sequence):

```r
devtools::document()        # regenerate NAMESPACE + man/ from roxygen
devtools::check()           # R CMD check
devtools::build()           # build tarball
devtools::install()         # install locally
```

Run the app (must be executed from the **project root** so relative cache dirs resolve):

```r
AEDi::run_app()
```

Initialize / rebuild the backend database schema:

```r
prepare_db()                                # SQLite (default): creates aedidb.sqlite
prepare_db(type = "pgsql", userdb = ...,    # PostgreSQL + PostGIS
           passwddb = ..., hostdb = ...)
```

Run tests:

```r
devtools::test()                            # testthat suite (golem recommended tests)
```

Render the pkgdown site (output is gitignored under `docs/`):

```r
pkgdown::build_site()
```

Build README (edit `README.Rmd`, **not** `README.md`):

```r
knitr::knit("README.Rmd")
```

> **Note:** `devhist.R` is the historical development script — it is excluded from the
> build (`.Rbuildignore`) but documents how the package was scaffolded.

## Architecture & Control Flow

### Shiny app structure

```
run_app()  (R/run_app.R)
  ├── ensures cache dirs exist: coleta/, manipula/, documenta/, visualiza/ (each + /cache)
  └── shinyApp(ui = app_ui(), server = app_server)

app_ui()  (R/ui_app.R)
  ├── add_external_resources()  → loads www/styles.css, www/custom.js, shinyjs, sweetalert
  └── shinydashboardPlus::dashboardPage
        ├── header    = header_ui()        (R/ui_header.R)        → logo + header_buttons
        ├── sidebar   = sidebar_ui()       (R/ui_sidebar.R)       → nav menu (mostly placeholders)
        ├── body      = body_ui()          (R/ui_body.R)          → upload_data_ui("data")
        └── controlbar= right_sidebar_ui() (R/ui_rightbar.R)      → help panel

app_server()  (R/server_app.R)
  ├── upload_data_server("data")   ← the core module
  └── callModule(header_buttons, "header")
```

Only the **"Upload de Dados-Fontes"** tab (`tabName = "upload_data"`) is wired to
content. The other sidebar items (Diagnóstico, Dicionário, Insights, Modelagem) are
placeholders not yet implemented.

### The core module: `upload_data_module.R`

`upload_data_ui` renders a `tabBox` with four tabs:

1. **Inserção de Fonte** — pick a source type, generate/fetch data, write to DB.
2. **Tabela de Dados** — preview imported data in a `DT::datatable`.
3. **Resumo** — `summarytools::dfSummary` rendering.
4. **Variáveis e Indicadores** — drag-and-drop indicator builder using `sortable`,
   a `shinymath::mathInput` LaTeX equation, and `latex2r` to convert to R code.

### Source-type submodules (nested via `parent_session`)

Each data source has its own `*_ui` / `*_server` pair in `R/upload_<source>.R`. They
are dynamically injected inside `upload_data_module` based on `input$sourcetype`:

| `sourcetype` | Source | File | Generates |
|---|---|---|---|
| 5 | IBGE SIDRA | `upload_sidraibge.R` | `sidra::sidra(...)` call string |
| 11 | DATASUS | `upload_datasus.R` | `datasus::<func>(...)` call string |
| 12 | RAIS (PostgreSQL) | `upload_raispsql.R` | RAIS query call string |
| 13 | INEP / IDEB | `upload_inep.R` | `educabR::le_ideb(...)` call string |
| 1/2/9 | URL / local upload / server file | inline in `upload_data_module.R` | file path |

**Key pattern:** Submodules do *not* fetch data themselves. They build an **R
expression as a text string** and write it into the parent module's `upload_file`
input via `updateTextInput(session = parent_session, "upload_file", value = ...)`.
The parent's `selected_files()` reactive then does `eval(parse(text = input$upload_file))`
to actually execute it. New source types must follow this contract.

### Data write pipeline

`db_datawrite()` (`R/dbdatawriter.R`) is the single sink for new data:

1. **Sanitize locals** — matches `local` (IBGE code or name) against the `local` table
   to resolve `local_id`.
2. **Sanitize dates** — coerces `periodo` to `Date` via `%Y`, `%Y-%m`, or `%Y-%m-%d`;
   annual data (`data_freq_id == 9`) is forced to Dec 31.
3. **Append** to `mdata`, `mdata_exts`, `mdata_timetable`, and `data_values`.
4. **Refresh materialized views** (`named_datavalues`, `geonamed_datavalues`).

## Database

### Backends

- **PostgreSQL + PostGIS** is the production backend (used by the live app and by
  `db_datawrite`).
- **SQLite** is supported by `prepare_db(type = "sqlite")` for local schema
  bootstrapping, but `db_datawrite` is PostgreSQL-only.

### Connection / environment variables (from `.Renviron`)

The app reads DB credentials via `Sys.getenv()`. A `.Renviron` file in the project
root provides them. **The app will not start without these set.**

| Variable(s) | Used for | Where |
|---|---|---|
| `dbname`, `user`, `password`, `host` | Main AEDi PostgreSQL DB | `upload_data_module.R`, `dbdatawriter.R` |
| `mte_rais`, `dbrais`, `pwdrais`, `hostraispsql` | Separate RAIS PostgreSQL DB | `upload_raispsql.R` |
| `tdbname`, `userdb`, `passwddb`, `hostdbdev`, `passwddbdev` | `prepare_db()` dev setup | `dbprepare.R`, `create_extend_geogroup_view.R` |

> `.Renviron` is untracked. Do not commit real credentials.

### Core schema (defined in `R/dbprepare.R`)

Data is stored in **long/tidy format** and denormalized via materialized views:

- `data_values(mdata_id, local_id, refdate, value)` — the fact table (composite PK).
- `mdata` / `mdata_exts` / `mdata_timetable` — indicator metadata (name, class,
  frequency, source, last-update tracking).
- `local` / `geoloc` / `local_group` — geographic entities (`geoloc.geometry` stores
  PostGIS geometries; `local_id` follows IBGE codes: <10 = region, <100 = state,
  6-digit = municipality).
- `datagroup` / `group_parent` / `mdata_group` — hierarchical grouping of indicators
  and localities (e.g. biomes, legal-Amazon, semiarid).
- `datasource` / `datasource_type` / `institution` / `officialer` — provenance.
- `data_class` / `data_freq` / `data_type` — classification lookups.

**Materialized views** (refresh after writes):

- `named_datavalues` — joins `data_values` → `mdata` → `mdata_exts` → `datasource`.
- `geonamed_datavalues` — further joins `local` → `recortes_geograficos`.
- `recortes_geograficos` — built by `create_extend_geogroup_view.R`; one row per
  municipality with centroid coords, UF, region, and group memberships.

ER/EER diagrams live in `inst/app/www/` (`v2024-12-EER.png`, `*ERpsql*.png`).

## Code Organization

| Path | Purpose |
|---|---|
| `R/` | Package source — the Shiny app, modules, DB helpers, utils. **This is what ships.** |
| `coleta/` | "Coleta" (collection): ~100 ad-hoc `.R` scripts that compute indicators from cached data, plus `cache/` with downloaded raw datasets (DBC, CSV, XLSX, GPKG). **Not part of the package build** — these are analysis/ETL scripts run against the working copy. |
| `coleta/cache/` | Raw data organized by source (e.g. `datasus_cnes_*`, `rais_vinculos_*`, `infra*_aedi`, `objetivo*_via_aedi`). |
| `manipula/` | "Manipula" (manipulation): transformation pipeline outputs; `metadados/` holds per-source variable lists. |
| `documenta/`, `visualiza/` | Documentation / visualization staging (each has a `cache/`). |
| `inst/app/www/` | Static assets served at `/www` (CSS, JS, logos, schema PNGs). |
| `inst/extdata/` | Demo/example CSVs used by the in-app file picker ("Demo Data" volume). |
| `inst/rmarkdown/templates/` | R Markdown template ("Data Validation Report"). |
| `data/` | Lazy-loaded package data (`.rda`): `agregados`, `raismetalayoute`, `raismetalayoutv`, `sidrameta`. |
| `data-raw/` | Scripts that produce `data/*.rda` (e.g. `data_sidra.R`). |
| `modprov/` | Standalone prototype app for testing individual modules in isolation. |
| `tests/testthat/` | Golem-recommended smoke tests (UI is a taglist, server is a function, app launches). |
| `man/`, `docs/` | Auto-generated Rd and pkgdown HTML — do not edit by hand. |

## Conventions

- **Roxygen2 with markdown** (`RoxygenNote: 7.3.2`, `Roxygen: list(markdown = TRUE)`).
  `NAMESPACE` and `man/` are generated — never hand-edit.
- **2-space indentation**, UTF-8, spaces for tabs (per `AEDi.Rproj`).
- **`Collate` order in `DESCRIPTION`** controls file load order and **matters** (utils
  define `agregfunc`, helpers define `flucol`, etc., before modules use them). If you
  add an `R/` file, update `Collate` via `devtools::document()`.
- **Shiny modules** use the `moduleServer(id, function(input, output, session))`
  pattern with `ns <- session$ns`. UI functions take `id`; nested submodules take an
  extra `parent_session` argument.
- **Native pipe `\()`** for anonymous functions and `|>` for chaining are used
  throughout (requires R >= 4.1).
- **Helper functions** (`flucol`, `icon_text`, `rep_br`, `insert_logo`) live in
  `R/ui_helpers.R`. Stat helpers (`somasna`, `mediasna`, `mmov`, etc.) and the
  `agregfunc` vector (user-facing aggregation labels) live in `R/utils.R`.
- **Logging:** `futile.logger` is used to log indicator-creation provenance to
  `coleta/<indicator>.R.log` and into the generated script file.

## Gotchas

### Undeclared runtime dependencies

`NAMESPACE` declares imports and several `R/` files `library()` packages that are
**not listed in `DESCRIPTION`**. These must be installed for the app to run (and
`R CMD check` will flag them). Known culprits:

`RPostgreSQL`, `RPostgres`, `datasus`, `educabR`, `latex2r`, `futile.logger`,
`tidyr`, `sf`, `lubridate`, `stringr`, `rvest`, `xml2`, `httr`, `jsonlite`,
`janitor`.

When adding code that uses a new package, add it to `Imports`/`Suggests` via
`usethis::use_package()`.

### `upload_raispsql.R` connects at source time

The module-level `#module 'global'` block in `upload_raispsql.R` runs `DBI::dbConnect`
and queries the RAIS PostgreSQL DB **when the file is sourced** (i.e. on package load
if the module is invoked). If the RAIS DB or its env vars (`mte_rais`, `dbrais`,
`pwdrais`, `hostraispsql`) are unavailable, selecting source type 12 will error.

### `prepare_db()` clears the session

`R/dbprepare.R` begins with `rm(list = ls())`. Never `source()` it into an
interactive session with unsaved work — call `prepare_db()` as a function instead.

### `eval(parse(text = ...))` is pervasive

Submodules emit R code as strings and the parent module `eval`s it. This is
intentional (it lets users see/edit the exact call) but means **untrusted input
becomes executed R code**. Treat the `upload_file` input as code, not data.

### Materialized views need refreshing

After any direct write to `data_values` / `mdata` (outside `db_datawrite`), run:

```sql
REFRESH MATERIALIZED VIEW named_datavalues;
REFRESH MATERIALIZED VIEW geonamed_datavalues;
```

Otherwise the app's indicator picker (which queries `geonamed_datavalues`) won't see
new data.

### Cache directories are created at runtime

`run_app()` creates `coleta/cache`, `manipula/cache`, `documenta/cache`,
`visualiza/cache` under the current working directory if missing. Run the app from
the project root, not from `inst/` or a temp dir.

### IBGE local_id encoding

`local_id` / `geoloc_id` follow IBGE conventions: single-digit = macro-region,
two-digit = UF (state), seven-digit (first six = municipality code) = municipality.
`db_datawrite` matches incoming `local` values (numeric IBGE codes or
`"CODE Name"` strings) against the `local` table. Unmatched locals are silently
dropped from the write.

## Testing

Tests are minimal — `tests/testthat/test-golem-recommended.R` checks that `app_ui()`
returns a shinytaglist, `app_server` is a function, and the app process launches.
There is **no unit coverage** for the DB layer or modules; the "app launches" test
spawns a subprocess via `processx` and checks it stays alive. To run:

```r
devtools::test()
```
