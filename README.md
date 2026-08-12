# valida-bio

Aplicação web (R + Shiny, PT-BR) que padroniza listas de espécies para o padrão
**Darwin Core** e valida os nomes contra as autoridades brasileiras (**Flora do
Brasil**, **Fauna do Brasil**), com fallback ao vivo no **backbone do GBIF**.
Cruza status de conservação (**MMA + IUCN**), executa uma **verificação
geográfica** (buffer de 10 km + ocorrências GBIF + estado/bioma →
`distributionFlag`), mostra um **mapa interativo** e gera **dois Excel** (dados
padronizados + auditoria).

O pacote R se chama `ObservaBio`. A aplicação é apresentada como **ZHOUSE** na
interface.

## Requisitos

R >= 4.1.0. Dependências em `DESCRIPTION`, versões fixadas em `renv.lock`
(`faunabr` vem do GitHub via `Remotes:`; o GBIF é consultado ao vivo por `rgbif`,
sem `taxadb`).

## Rodar localmente

```r
pkgload::load_all(".")
run_app()
```

## Validar

```sh
Rscript --vanilla -e "pkgload::load_all('.', quiet = TRUE); cat('load_all OK\n')"
Rscript --vanilla -e "devtools::test()"
```

## Publicar

Antes de qualquer publicação, regenere os artefatos de deploy — eles alimentam
as duas rotas do Posit Connect Cloud a partir de uma única lista de arquivos:

```sh
Rscript data-raw/build_deploy.R          # renv.lock + manifest.json + .posit/*.toml
Rscript data-raw/build_deploy.R --check  # confere se o lock ainda está atual
```

A publicação em si é pela IDE (Posit Publisher / `rsconnect::deployApp()`), que
lê `renv.lock` e `.posit/publish/valida-bio.toml`. A rota por GitHub, que leria
`manifest.json`, não é usada.

## Estrutura

| Caminho | O quê |
| --- | --- |
| `R/utils_*.R` | Lógica pura: cascata, Darwin Core, geoverificação, export |
| `R/provider_*.R` | Bases taxonômicas sob o contrato de provedor |
| `R/mod_*.R` | Módulos Shiny — só ponte entre UI/reatividade e as funções puras |
| `R/app_server.R` | Orquestrador: liga os módulos, sem regra de negócio |
| `inst/extdata/` | Bases de referência embarcadas (recorte Brasil, offline) |
| `data-raw/*.R` | Scripts que regeneram as bases, o CSS e os artefatos de deploy |
| `tests/testthat/` | Suíte de testes |

Para incluir uma nova base taxonômica, veja o contrato em
`R/provider_contract.R`: um `provider_<nome>.R` mais uma linha em
`R/provider_registry.R`, sem tocar no motor da cascata.

## Documentação de projeto

O registro de decisões (ADRs), as lições aprendidas e a especificação **não são
versionados** — ficam em `docs/` e `SPEC.md` apenas nesta máquina, por decisão do
autor. Não há backup deles no GitHub.
